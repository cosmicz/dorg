;;; dorg-api.el --- Public mutate DSL, typed patch IR v2, command envelope -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The public v0.1 write surface, sitting ABOVE the read
;; engine (`dorg-ql') and the typed write engine (`dorg-mutate').  It is the
;; only public mutation entry for Clime/CLI/serve.  Canonical specification:
;; docs/mutate.org, docs/api.org, docs/query.org.
;;
;; One normalization path serves dry-run and apply:
;;
;;   public DSL form  (on …)/(insert (heading …) …)/(batch …)
;;     -> `dorg-api-read'        parse + shape-validate
;;     -> `dorg-api-normalize'   lower each action to TYPED PATCH IR v2;
;;                               expand (where QUERY) via `dorg-ql-select',
;;                               except late-bound structural `(key V)' hosts
;;     -> IR-native engine boundary (tracked-regression):
;;          `dorg-mutate--plan-patches'  (dry-run)
;;          `dorg-mutate--apply-patches' (apply, + safety gates)
;;        the API speaks TYPED PATCH IR directly to the engine.
;;     -> canonical response (adds :intent + :patches)
;;
;; Typed patch IR v2 is the canonical semantic mutation IR from normalization
;; through execution.  There is no second operation language or lowering seam.

;;; Code:

(require 'cl-lib)
(require 'dorg-error)
(require 'dorg-id)
(require 'dorg-model)
(require 'dorg-mutate-grammar)
(require 'dorg-mutate)
(require 'dorg-ext)
(require 'dorg-ql)
(require 'dorg-registry)

;;; ─── Errors ──────────────────────────────────────────────────────────────

(define-error 'dorg-api-error "dorg API command failed" 'dorg-error)

(defconst dorg-api-error-reasons
  ;; engine reasons (shared with `dorg-mutate-error') + command-envelope reasons
  (append dorg-mutate-error-reasons
          '(mode-error expected-targets-required target-set-changed
            broad-write-refused identity-violation
            ;; `malformed' is raised by the mutation-intent constructor when an
            ;; option (incl. the retired `:extension-inputs' key) is rejected as
            ;; foreign authority.
            malformed))
  "Closed set of `:reason' symbols a `dorg-api-error' may carry.")

(defun dorg-api--raise (reason message &rest data)
  "Signal `dorg-api-error' with REASON, MESSAGE, and extra DATA plist.
Condition data is (MESSAGE PLIST) with PLIST beginning `:reason' REASON —
mirrors `dorg-mutate--raise' so the accessors are interchangeable."
  (cl-assert (memq reason dorg-api-error-reasons))
  (signal 'dorg-api-error
          (list message (apply #'list :reason reason data))))

(defun dorg-api-error-data (err)
  "Return the structured data plist of a `dorg-api-error' condition ERR.
Delegates to the generic `dorg-error-data' (F2 consolidation) after
pinning the leaf symbol, so per-symbol isolation is preserved while the
plist extraction lives in one place."
  (and (eq (car-safe err) 'dorg-api-error)
       (dorg-error-data err)))

(defun dorg-api-error-reason (err)
  "Return the `:reason' symbol of a `dorg-api-error' condition ERR."
  (plist-get (dorg-api-error-data err) :reason))

;;; ─── Config ──────────────────────────────────────────────────────────────

(defcustom dorg-api-broad-write-threshold 100
  "Max confirmed targets a query-scoped apply may touch without `--allow-broad'.
An apply over more than this many targets signals `broad-write-refused'
unless the caller opts in with `:allow-broad'."
  :type 'integer
  :group 'dorg)

;;; ─── Host / value helpers ────────────────────────────────────────────────

(defun dorg-api--host (dsl-target)
  "Return the typed HOST for a DSL target.
(id N) becomes (:heading :id N); public `document' becomes internal :root.
Signals `invalid-target' otherwise."
  (pcase dsl-target
    ('document :root)
    (`(id ,id)
      (unless (or (integerp id)
                  (and (stringp id) (string-match-p "\\`[0-9]+\\'" id)))
        (dorg-api--raise 'invalid-target "invalid heading id" :target dsl-target))
      (list :heading :id (if (stringp id) (string-to-number id) id)))
    ('root
     (dorg-api--raise 'invalid-target
                      "public root is retired; use document"
                      :target dsl-target))
    (_ (dorg-api--raise 'invalid-target "invalid target" :target dsl-target))))

(defun dorg-api--tget (tagged key)
  "Return KEY's value from a TAGGED target/host list (TAG :k v …) — i.e. the
plist after the leading type tag."
  (plist-get (cdr tagged) key))

(defun dorg-api--host-id (host)
  "Return the concrete integer id of HOST, or refuse the document host."
  (if (and (consp host) (eq (car host) :heading))
      (dorg-api--tget host :id)
    (dorg-api--raise 'invalid-target "document is not a heading target" :host host)))

(defun dorg-api--extern-heading-host-p (host)
  "Return non-nil when HOST is a typed external-key heading host."
  (and (consp host) (eq (car host) :heading) (eq (cadr host) :extern)))

(defun dorg-api--heading-target (kind host)
  "Return a KIND heading/subtree typed target for HOST.
Concrete id hosts keep the historical `:id' shape; external-key hosts carry the
same inline `:extern' identity plist as heading hosts."
  (cond
   ((eq host :root)
    (dorg-api--raise 'invalid-target "document is not a heading target" :host host))
   ((dorg-api--extern-heading-host-p host)
    (cons kind (cdr host)))
   ((and (consp host) (eq (car host) :heading)
         (integerp (dorg-api--tget host :id)))
    (list kind :id (dorg-api--tget host :id)))
   (t (dorg-api--raise 'invalid-target "invalid heading target" :host host))))

(defun dorg-api--structural-host (dsl-target source)
  "Return the typed host for a structural DSL target under SOURCE.
Nested operands accept direct `(key V)' identity alongside `(id N)' and public
`document'.  Query wrappers are confined to outer `(on (where Q) ...)'."
  (pcase dsl-target
    (`(key ,_)
     (car (dorg-api--key-target-hosts dsl-target source)))
    (`(where . ,_)
     (dorg-api--raise 'invalid-target
                      "structural targets require direct id, key, or document"
                      :target dsl-target))
    (_ (dorg-api--host dsl-target))))

(defun dorg-api--anchor-value (dsl-target source)
  "Return the typed sibling host for DSL-TARGET under SOURCE.
Numeric and external-key anchors both retain their typed heading host through
native validation.  `document' is never a sibling."
  (let ((host (dorg-api--structural-host dsl-target source)))
    (cond
     ((eq host :root)
      (dorg-api--raise 'invalid-target "document is not a sibling anchor"
                       :host host :target dsl-target))
     ((dorg-api--extern-heading-host-p host) host)
     ((and (consp host) (eq (car host) :heading)
           (integerp (dorg-api--tget host :id)))
      host)
     (t (dorg-api--raise 'invalid-target "invalid sibling anchor"
                         :host host :target dsl-target)))))

(defun dorg-api--string (v slot)
  "Return V when it is a string, else signal `invalid-target' naming SLOT."
  (unless (stringp v)
    (dorg-api--raise 'invalid-target (format "%s must be a string" slot)
                     :slot slot :value v))
  v)

(defun dorg-api--position (position allowed)
  "Return the typed keyword for public POSITION when it is in ALLOWED.
ALLOWED is a list of public one-symbol position forms such as
`(before-first)' and `(after-last)'."
  (if (member position allowed)
      (intern (concat ":" (symbol-name (car position))))
    (dorg-api--raise 'invalid-target "unsupported insertion position"
                     :position position :allowed allowed)))

(defun dorg-api--check-property-key (k)
  "Reject the identity property DORG_ID (protected metadata)."
  (dorg-api--string k :key)
  (when (equal k dorg-id-property)
    (dorg-api--raise 'invalid-target "DORG_ID is identity, not a property" :key k))
  k)

(defun dorg-api--check-keyword-key (k)
  "Reject DORG_-prefixed keywords (protected metadata)."
  (dorg-api--string k :key)
  (when (dorg-protected-keyword-p k)
    (dorg-api--raise 'invalid-target "DORG_-prefixed keywords are protected" :key k))
  k)

(defun dorg-api--occurrence (spec)
  "Validate public one-based (occurrence N) SPEC and return N.
N must be a positive integer; otherwise signal `invalid-target'."
  (unless (and (consp spec) (eq (car spec) 'occurrence)
               (integerp (nth 1 spec)) (>= (nth 1 spec) 1))
    (dorg-api--raise 'invalid-target "occurrence must be a positive integer (>= 1)"
                     :occurrence spec))
  (nth 1 spec))

(defun dorg-api--drawer-line-name (name)
  "Validate and return a public drawer-line NAME.
NAME is preserved exactly for model addressing.  Org drawer markers are
case-insensitive for structural purposes, so PROPERTIES and END are protected
under every spelling."
  (dorg-api--string name :name)
  (unless (string-match-p "\\`[_[:word:]-]+\\'" name)
    (dorg-api--raise 'invalid-target
                     "drawer name must match [_[:word:]-]+"
                     :name name))
  (when (member (upcase name) '("PROPERTIES" "END"))
    (dorg-api--raise 'invalid-target
                     "PROPERTIES and END are not mutable generic drawers"
                     :name name))
  name)

(defun dorg-api--drawer-raw-line (line)
  "Validate and return one public drawer raw LINE.
The value must be one physical line and must not itself be a complete drawer
opener or terminator after optional horizontal whitespace."
  (dorg-api--string line :value)
  (when (string-match-p "[\0\r\n]" line)
    (dorg-api--raise 'invalid-target
                     "drawer line must contain exactly one physical line"
                     :value line))
  (when (string-match-p "\\`[ \t]*:[_[:word:]-]+:[ \t]*\\'" line)
    (dorg-api--raise 'invalid-target
                     "drawer line must not be a complete drawer marker"
                     :value line))
  line)

;;; ─── Transport-safe identity token ──────────────────────────────────────
;; A target's identity is either an integer heading id (id-bearing sources) or
;; an extern key (:extern :key K :value V) for a faithful DORG_ID-less binding.
;; Both must round-trip through the dry-run/apply safety envelope across Elisp,
;; JSON, CLI, and HTTP as ONE canonical wire string, and mixed sets must sort
;; deterministically.  Integer id -> its decimal string; extern -> a
;; `key:<pct(K)>=<pct(V)>' string whose reserved characters (`% , : =' plus
;; whitespace) are percent-encoded so neither a comma nor a whitespace split can
;; corrupt a quoted value.

(defconst dorg-api--token-reserved '(?% ?, ?: ?= ?\s ?\t ?\n)
  "Characters percent-encoded in an extern token so CSV/whitespace splits are safe.
`%' is listed first in intent; `dorg-api--pct-encode' encodes it like any other.")

(defun dorg-api--pct-encode (s)
  "Percent-encode the reserved characters of string S (`%' included)."
  (mapconcat (lambda (c)
               (if (memq c dorg-api--token-reserved)
                   (format "%%%02X" c)
                 (char-to-string c)))
             (string-to-list s) ""))

(defun dorg-api--pct-decode (s)
  "Reverse `dorg-api--pct-encode' on string S."
  (replace-regexp-in-string
   "%\\([0-9A-Fa-f]\\{2\\}\\)"
   (lambda (m) (char-to-string (string-to-number (match-string 1 m) 16)))
   s t t))

(defun dorg-api-target-token (target)
  "Return the canonical transport wire string for identity TARGET.
Integer id -> its decimal string; extern host (:extern :key K :value V) ->
`key:<pct(K)>=<pct(V)>'; a string is assumed already canonical."
  (cond
   ((integerp target) (number-to-string target))
   ((and (consp target) (eq (car target) :extern))
    (format "key:%s=%s"
            (dorg-api--pct-encode (plist-get (cdr target) :key))
            (dorg-api--pct-encode (plist-get (cdr target) :value))))
   ((stringp target) target)
   (t (dorg-api--raise 'invalid-target "un-tokenizable target identity"
                       :target target))))

(defun dorg-api-parse-target-token (s)
  "Parse a wire token S into an identity value: an integer id or extern host."
  (cond
   ((and (stringp s) (string-match-p "\\`[0-9]+\\'" s)) (string-to-number s))
   ((and (stringp s) (string-match "\\`key:\\([^=]*\\)=\\(.*\\)\\'" s))
    (list :extern
          :key (dorg-api--pct-decode (match-string 1 s))
          :value (dorg-api--pct-decode (match-string 2 s))))
   (t (dorg-api--raise 'invalid-target "unparseable target token" :token s))))

(defun dorg-api--dedup-identities (ids)
  "Return IDS with duplicates (by canonical token) removed, order preserved."
  (let ((seen (make-hash-table :test 'equal)) (out nil))
    (dolist (id ids)
      (let ((k (dorg-api-target-token id)))
        (unless (gethash k seen) (puthash k t seen) (push id out))))
    (nreverse out)))

(defun dorg-api--sort-identities (ids)
  "Return IDS ordered by canonical token (total order over mixed id+extern)."
  (sort (copy-sequence ids)
        (lambda (a b) (string< (dorg-api-target-token a) (dorg-api-target-token b)))))

;;; ─── Action → typed patch ────────────────────────────────────────────────

(defconst dorg-api--heading-fields
  (mapcar #'car dorg-mutate-grammar-heading-fields)
  "Intrinsic heading fields accepted by the public set production.")
(defconst dorg-api--planning-fields '(scheduled deadline closed))

(defun dorg-api--anchor (a source)
  "Return (:before HOST) / (:after HOST) tail for a move/insert anchor A.
A is (before TARGET) or (after TARGET); nil when absent."
  (pcase a
    (`(before ,tgt) (list :before (dorg-api--anchor-value tgt source)))
    (`(after ,tgt)  (list :after  (dorg-api--anchor-value tgt source)))
    (_ (dorg-api--raise 'invalid-target "invalid sibling anchor" :anchor a))))

(defun dorg-api--item-post-blank (form)
  "Split item FORM's optional trailing `:post-blank N'; return (HEAD . N).
Every public item form accepts one optional trailing `:post-blank N'
\(default 0) — the item's persisted trailing-blank-line count, which
participates in the items digest and crosses the candidate-model wire
\(tracked-regression).  N must be a non-negative integer.  HEAD is FORM without
the tail."
  (let ((len (and (proper-list-p form) (length form))))
    (if (and len (>= len 2) (eq (nth (- len 2) form) :post-blank))
        (let ((n (nth (1- len) form)))
          (unless (and (integerp n) (>= n 0))
            (dorg-api--raise 'invalid-target
                             "item :post-blank must be a non-negative integer"
                             :value n :form form))
          (cons (butlast form 2) n))
      (cons form 0))))

(defun dorg-api--item-value (form)
  "Lower one public item FORM to a normalized item-value plist.
FORM is one of (text \"S\") | (keyword \"K\" \"V\")
| (planning :scheduled S? :deadline D? :closed C?)
| (drawer \"NAME\" (K V)…) | (drawer \"NAME\" :lines (\"L\"…)),
each accepting one optional trailing `:post-blank N'
\(`dorg-api--item-post-blank'); the normalized value always carries
`:post-blank'.  This ONE grammar is also the candidate-model wire's item
vocabulary (tracked-regression): every wire form parses here and round-trips
through `dorg-mutate--item-from-value'.  Keyword keys are checked for
protected metadata; the engine re-validates the returned value on apply."
  (pcase-let ((`(,head . ,post-blank) (dorg-api--item-post-blank form)))
    (append
     (pcase head
       (`(text ,s)
        (list :kind 'text :text (dorg-api--string s :text)))
       (`(keyword ,k ,v)
        (dorg-api--check-keyword-key k)
        (list :kind 'keyword :keyword k :value (dorg-api--string v :value)))
       (`(planning . ,kv)
        (append (list :kind 'planning)
                (cl-loop for (key val) on kv by #'cddr
                         unless (memq key '(:scheduled :deadline :closed))
                         do (dorg-api--raise 'invalid-target
                                             "planning item field must be :scheduled/:deadline/:closed"
                                             :field key)
                         append (list key val))))
       (`(drawer ,name :lines ,lines)
        (dorg-api--string name :name)
        (list :kind 'drawer :name name :lines lines))
       (`(drawer ,name . ,props)
        (dorg-api--string name :name)
        (list :kind 'drawer :name name
              :properties (mapcar
                           (lambda (kv)
                             (unless (and (consp kv) (= (length kv) 2))
                               (dorg-api--raise 'invalid-target
                                                "drawer property must be (KEY VALUE)"
                                                :property kv))
                             (cons (dorg-api--string (nth 0 kv) :key)
                                   (dorg-api--string (nth 1 kv) :value)))
                           props)))
       (_ (dorg-api--raise 'invalid-op "invalid item form" :form form)))
     (list :post-blank post-blank))))

(defun dorg-api--action->patch (host action source)
  "Lower one public ACTION under HOST to a typed patch plist."
  (pcase action
    (`(set (property ,k) ,v)
     (list :op 'put :target (list :property :host host :name (dorg-api--check-property-key k))
           :value (dorg-api--string v :value)))
    (`(delete (property ,k))
     (list :op 'delete :target (list :property :host host :name (dorg-api--check-property-key k))))
    (`(replace (keywords ,k) . ,vs)
     (dorg-api--check-keyword-key k)
     (unless (and vs (cl-every #'stringp vs))
       (dorg-api--raise 'invalid-target
                        "replace (keywords KEY) needs >=1 string value"
                        :key k :values vs))
     (list :op 'replace :target (list :keywords :host host :name k) :values vs))
    (`(delete (keywords ,k))
     (list :op 'delete :target (list :keywords :host host :name (dorg-api--check-keyword-key k))))
    (`(replace (keyword ,k ,occ) ,v)
     (dorg-api--check-keyword-key k)
     (list :op 'replace
           :target (list :keyword :host host :name k :occurrence (dorg-api--occurrence occ))
           :value (dorg-api--string v :value) :missing :error))
    (`(delete (keyword ,k ,occ))
     (dorg-api--check-keyword-key k)
     (list :op 'delete
           :target (list :keyword :host host :name k :occurrence (dorg-api--occurrence occ))
           :missing :error))
    (`(insert (keyword ,k) ,position ,v)
     (dorg-api--check-keyword-key k)
     (list :op 'insert :target (list :keywords :host host :name k)
           :value (dorg-api--string v :value)
           :position (dorg-api--position position
                                         '((before-first) (after-last)))))
    (`(insert (drawer-line ,name ,occurrence) ,position ,line)
     ;; This capability intentionally addresses only the first same-name
     ;; drawer.  Keep the public one-based selector explicit, then freeze its
     ;; accepted value and position before constructing the typed IR.
     (dorg-api--host-id host)
     (let ((occurrence-number (dorg-api--occurrence occurrence)))
       (unless (= occurrence-number 1)
         (dorg-api--raise 'invalid-target
                          "drawer-line insertion currently supports only occurrence 1"
                          :occurrence occurrence))
       (list :op 'insert
             :target (list :drawer-lines :host host
                           :name (dorg-api--drawer-line-name name)
                           :occurrence occurrence-number)
             :position (dorg-api--position position
                                           '((before-first) (after-last)))
             :missing :create
             :value (dorg-api--drawer-raw-line line))))
    (`(set (heading ,field) ,v)
     (unless (memq field dorg-api--heading-fields)
       (dorg-api--raise 'invalid-target "unknown heading field" :field field))
     (list :op 'update :target (dorg-api--heading-target :heading host)
           :fields (list (intern (concat ":" (symbol-name field))) v)))
    (`(set (planning ,field) ,v)
     (unless (memq field dorg-api--planning-fields)
       (dorg-api--raise 'invalid-target "unknown planning field" :field field))
     (list :op 'update :target (list :planning :host host)
           (intern (concat ":" (symbol-name field))) v))
    (`(move (subtree) (under ,tgt . ,anchors))
     (unless (proper-list-p anchors)
       (dorg-api--raise 'invalid-target
                        "move anchors must form a proper list" :anchors anchors))
     (when (> (length anchors) 1)
       (dorg-api--raise 'invalid-target
                        "supply at most one of before/after" :anchors anchors))
     (list :op 'move :target (dorg-api--heading-target :subtree host)
           :to (append (list :under (dorg-api--structural-host tgt source))
                       (when anchors (dorg-api--anchor (car anchors) source)))))
    (`(delete (heading))
     (list :op 'delete :target (dorg-api--heading-target :heading host)))
    (`(delete (subtree))
     (list :op 'delete :target (dorg-api--heading-target :subtree host)))
    (`(insert (text) ,position ,text)
     ;; Text insertion addresses the collection and keeps placement explicit.
     ;; Root has no public prose body; reject it during normalization.
     (dorg-api--host-id host)
     (list :op 'insert :target (list :texts :host host)
           :position (dorg-api--position position
                                         '((before-first) (after-last)))
           :value (dorg-api--string text :value)))
    (`(insert (text) . ,_)
     (dorg-api--raise 'invalid-target
                      "text insertion requires (text), POSITION, TEXT"
                      :action action))
    (`(insert (text . ,_) . ,_)
     (dorg-api--raise 'invalid-target
                      "text insertion address must be exactly (text)"
                      :action action))
    (`(replace (text ,occurrence) ,text)
     ;; Replacement is a strict single-occurrence edit.  Preserve the public
     ;; one-based selector in typed IR; only occurrence one is supported today.
     (dorg-api--host-id host)
     (let ((number (dorg-api--occurrence occurrence)))
       (unless (= number 1)
         (dorg-api--raise 'invalid-target
                          "text replacement currently supports only occurrence 1"
                          :occurrence occurrence))
       (list :op 'replace
             :target (list :text :host host :occurrence number)
             :missing :error :value (dorg-api--string text :value))))
    (`(replace (text . ,_) . ,_)
     (dorg-api--raise 'invalid-target
                      "text replacement requires (text (occurrence 1)), TEXT"
                      :action action))
    (`(replace (items) . ,items)
     ;; Replace the heading's WHOLE ordered item sequence with an arbitrary mix
     ;; of item forms (the only public verb that can express a full sequence).
     ;; Normalizes to a typed whole-items replacement.  Root has no item body ->
     ;; rejected; an empty sequence is refused (use the item forms to set >=1).
     (dorg-api--host-id host)
     (unless items
       (dorg-api--raise 'invalid-target
                        "replace items needs >=1 item form (empty is not a clear)"
                        :action action))
     (list :op 'replace :target (list :items :host host)
           :items (mapcar #'dorg-api--item-value items)))
    (_
     (if (memq (car-safe action) '(set delete replace insert move))
         (dorg-api--raise 'invalid-target
                          "invalid or retired compound action address"
                          :action action)
       (dorg-api--raise 'invalid-op "unknown action" :action action)))))

(defun dorg-api--heading-constructor (clauses)
  "Normalize recursive public heading constructor CLAUSES.
`items' carries an ordered sequence of the existing public item forms;
`children' carries recursive `(heading CLAUSE...)' constructors.  Properties
remain coalesced into the leading PROPERTIES drawer."
  (unless (proper-list-p clauses)
    (dorg-api--raise 'invalid-target
                     "insert-heading clauses must form a proper list"
                     :clauses clauses))
  (let ((heading nil)
        (properties nil)
        (items nil)
        (children nil)
        (commented-seen nil)
        (items-seen nil)
        (children-seen nil))
    (dolist (clause clauses)
      (pcase clause
        (`(title ,v) (setq heading (plist-put heading :title v)))
        (`(todo ,v) (setq heading (plist-put heading :todo v)))
        (`(priority ,v) (setq heading (plist-put heading :priority v)))
        (`(tags . ,ts) (setq heading (plist-put heading :tags ts)))
        (`(commented ,v)
         (when commented-seen
           (dorg-api--raise 'invalid-target
                            "duplicate insert-heading commented clause"
                            :clause clause))
         (unless (memq v '(nil t))
           (dorg-api--raise 'invalid-target
                            "heading :commented must be boolean"
                            :clause clause))
         (setq commented-seen t
               heading (plist-put heading :commented v)))
        (`(property ,k ,v)
         (setq properties
               (append properties
                       (list (cons (dorg-api--check-property-key k)
                                   (dorg-api--string v :value))))))
        (`(items . ,forms)
         (when items-seen
           (dorg-api--raise 'invalid-target
                            "duplicate insert-heading items clause"
                            :clause clause))
         (unless (proper-list-p forms)
           (dorg-api--raise 'invalid-target
                            "insert-heading items must form a proper list"
                            :clause clause))
         (unless (cl-every (lambda (item)
                             (and (consp item)
                                  (memq (car item)
                                        '(text keyword planning drawer))))
                           forms)
           (dorg-api--raise 'invalid-target
                            "invalid insert-heading item form"
                            :clause clause))
         (setq items-seen t
               items (mapcar #'dorg-api--item-value forms)))
        (`(children . ,forms)
         (when children-seen
           (dorg-api--raise 'invalid-target
                            "duplicate insert-heading children clause"
                            :clause clause))
         (unless (proper-list-p forms)
           (dorg-api--raise 'invalid-target
                            "insert-heading children must form a proper list"
                            :clause clause))
         (setq children-seen t
               children
               (mapcar
                (lambda (child)
                  (pcase child
                    (`(heading . ,child-clauses)
                     (dorg-api--heading-constructor child-clauses))
                    (_ (dorg-api--raise
                        'invalid-target
                        "child constructor must be (heading CLAUSE...)"
                        :child child))))
                forms)))
        (_ (dorg-api--raise 'invalid-target
                            "invalid insert-heading clause" :clause clause))))
    (when properties
      (setq items
            (cons (list :kind 'drawer :name "PROPERTIES"
                        :properties properties :lines nil)
                  items)))
    (when (or items-seen items)
      (setq heading (plist-put heading :items items)))
    (when children-seen
      (setq heading (plist-put heading :children children)))
    heading))

(defun dorg-api--insert-heading->patch (form source)
  "Lower a top-level public recursive heading constructor FORM."
  (pcase form
    (`(insert (heading . ,clauses) (under ,tgt . ,anchors))
     (unless (and (proper-list-p anchors) (<= (length anchors) 1))
       (dorg-api--raise 'invalid-target
                        "insert heading accepts one optional sibling anchor"
                        :form form))
     (list :op 'insert
           :target (append
                    (list :tree :under (dorg-api--structural-host tgt source))
                    (and (car anchors)
                         (dorg-api--anchor (car anchors) source)))
           :heading (dorg-api--heading-constructor clauses)))
    (_ (dorg-api--raise 'invalid-target
                        "invalid or retired insert-heading form" :form form))))

;;; ─── Public form normalization ───────────────────────────────────────────

(defun dorg-api--project-hit (hit source &optional descriptive-where-p)
  "Project a `dorg-ql-select' HIT to a target host under SOURCE's identity policy.
Consult the single resolver `dorg-mutate--source-identity'.  A (:property K)
policy projects a hit carrying K to its stable EXTERN key.  Only when
DESCRIPTIVE-WHERE-P is non-nil may a hit with K absent project by an existing
numeric heading id; a hit with neither identity refuses.  A present malformed K
always refuses, so exact `(key V)' resolution never falls back to an id.  A
`dorg' policy projects to the heading id — and when that heading has NO DORG_ID
it is `unsupported'/`identity-provisioning-unavailable'.  Projection never
provisions identity."
  (let* ((policy (dorg-mutate--source-identity (dorg-mutate--resolve-source source)))
         (idkey (and (consp policy) (eq (car policy) :property) (cadr policy))))
    (if idkey
        (let* ((entry (assoc idkey (plist-get hit :properties)))
               (id (plist-get hit :heading-id)))
          (cond
           (entry
            (let ((val (cdr entry)))
              (unless (stringp val)
                (dorg-api--raise
                 'invalid-target
                 (format "matched heading has malformed identity key %S" idkey)
                 :key idkey :hit hit))
              ;; `:extern' carries a `t' value so the tail stays a valid plist.
              (list :heading :extern t :key idkey :value val)))
           ((and descriptive-where-p (integerp id))
            (list :heading :id id))
           (t
            (dorg-api--raise
             'invalid-target
             (if descriptive-where-p
                 (format "matched heading lacks identity key %S and DORG_ID" idkey)
               (format "matched heading lacks identity key %S" idkey))
             :key idkey :hit hit))))
      (let ((id (plist-get hit :heading-id)))
        (unless id
          (dorg-api--raise 'unsupported
                           "dorg identity requires a DORG_ID; provisioning is unavailable"
                           :detail 'identity-provisioning-unavailable :hit hit))
        (list :heading :id id)))))

(defun dorg-api--key-identity-spec (source)
  "Resolve the identity spec for a mutate `(key V)' target against SOURCE.
Consults the single resolver `dorg-mutate--source-identity'
(NO silent collapse):
a (:property K) policy -> (K . property); a `dorg' policy -> (nil . dorg); a
`none' policy -> `unsupported'/`identity-none' (read-only); a source that
resolves to NO binding -> a hard `invalid-target', never degraded to a guess."
  ;; `dorg-mutate--resolve-source' already signals `invalid-target' for a
  ;; no-binding/unrecognized source; let it propagate (no collapse to nil).
  (let* ((s (dorg-mutate--resolve-source source))
         (policy (dorg-mutate--source-identity s)))
    (pcase policy
      (`(:property ,k) (cons k 'property))
      ('dorg (cons nil 'dorg))
      ('none (dorg-api--raise 'unsupported
                              "(key V) is unavailable under identity=none"
                              :detail 'identity-none :source source)))))

(defun dorg-api--key-target-value (dsl-target)
  "Return the string VALUE from direct or query-composed key DSL-TARGET."
  (let ((value
         (pcase dsl-target
           (`(key ,v) v)
           (`(where (key ,v)) v)
           (_ (dorg-api--raise 'invalid-target
                               "key target must be exactly (key VALUE)"
                               :selector dsl-target)))))
    (dorg-api--string value :key-value)))

(defun dorg-api--key-target-hosts (dsl-target source)
  "Resolve direct or query-composed `(key V)' under the exactly-one contract.
Rewrites `(key V)' through the SHARED `dorg-ql--rewrite-key-refs' seam using the
policy spec, runs it, and enforces the identity contract: 0 matches -> under
`dorg' an identity/id selector cannot bootstrap an idless heading, so
`invalid-target'/`id-bootstrap-unsupported', else `invalid-target'/`missing';
>1 -> `identity-violation'; exactly one -> its projected host."
  (let* ((policy (dorg-mutate--source-identity (dorg-mutate--resolve-source source)))
         (spec (dorg-api--key-identity-spec source))
         (keyval (dorg-api--key-target-value dsl-target))
         (concrete (dorg-ql--rewrite-key-refs (list 'key keyval) spec))
         ;; Under `dorg', (key V) lowers to (id V): a NON-NUMERIC V is not a valid
         ;; id FORM, so it can never address an id-bearing heading — no read is
         ;; attempted (and no `dorg-ql-error' leaks); it is classified below like a
         ;; zero-match id.
         (dorg-invalid-id (and (eq policy 'dorg)
                               (not (and (stringp keyval)
                                         (string-match-p "\\`[0-9]+\\'" keyval)))))
         (hits (unless dorg-invalid-id (dorg-api--ql-select concrete source))))
    (cond
     ((and hits (cdr hits))
      (dorg-api--raise 'identity-violation
                       "identity key matches more than one heading"
                       :selector dsl-target :count (length hits)))
     (hits (list (dorg-api--project-hit (car hits) source)))
     ;; No id-bearing heading matched (zero hits, or a non-numeric `dorg' key).
     ;; SOURCE-DISTINGUISHING under `dorg': a source that CONTAINS an idless
     ;; heading is a BOOTSTRAP attempt; a source of only identified headings is an
     ;; ordinary missing.  A `(:property K)' key selector simply misses.
     ((and (eq policy 'dorg) (dorg-mutate--source-has-idless-heading-p source))
      (dorg-api--raise 'invalid-target
                       "(key V) under dorg cannot bootstrap an idless heading"
                       :detail 'id-bootstrap-unsupported :selector dsl-target))
     (t (dorg-api--raise 'invalid-target "no heading matches the identity key"
                         :detail 'missing :selector dsl-target)))))

(defun dorg-api--target-hosts (dsl-target source)
  "Resolve a DSL TARGET to a list of hosts, expanding (where QUERY) via SOURCE.
`(where QUERY)' keeps its ratified multi-target per-id expansion; the IDENTITY
PROJECTION of the matched set is policy-specific via the single resolver
`dorg-mutate--source-identity'.  Under (:property K), a keyed hit projects to
its extern key while a descriptive hit with K absent may use its existing
heading id; exact key selectors never take that fallback.  Under `dorg', hits
project by heading id.  A `none' policy is read-only: query-scoped targeting is
`unsupported'/`identity-none'.  Direct `(key V)' and query-composed
`(where (key V))' both carry the exactly-one identity contract."
  (pcase dsl-target
    (`(key ,_) (dorg-api--key-target-hosts dsl-target source))
    (`(where (key . ,_)) (dorg-api--key-target-hosts dsl-target source))
    (`(where ,_)
     (when (eq (dorg-mutate--source-identity (dorg-mutate--resolve-source source)) 'none)
       (dorg-api--raise 'unsupported "mutation targeting is unavailable under identity=none"
                        :detail 'identity-none :source source))
     (mapcar (lambda (hit) (dorg-api--project-hit hit source t))
             (dorg-api--ql-select (nth 1 dsl-target) source)))
    (_ (list (dorg-api--host dsl-target)))))

(defun dorg-api--ql-source (source)
  "Map a mutate SOURCE to the `dorg-ql-select' SOURCE argument.
SOURCE is a descriptor plist OR an already-resolved `dorg-mutate-source' struct
\(D4 threads the once-resolved binding through the query path).  Carries the
`:identity' policy through resolution (it is read by the single resolver, not
stripped).

A plain-org source returns its visiting buffer, or its file (a no-buffer
readable file is turned into a scoped owned query buffer by
`dorg-api--ql-select').  A Dolt source is queryable only through a registration
\(`dorg-ql-select' reaches Dolt via `dorg-ql--registration'), so this
synthesizes an UNREGISTERED `dorg-registration' from the SAME already-resolved
source — backend dolt-dir/root-id, file if any, and the resolved identity
policy — PREFERRED even when a buffer exists, so the resolved binding (not
implicit buffer state) drives the query.  No buffer is visited or created and
`dorg--registry' is never mutated."
  (cond
   ((null source) nil)
   ((bufferp source) source)
   ((or (dorg-mutate-source-p source)
        (and (consp source) (keywordp (car source))))
    (let ((resolved (dorg-mutate--resolve-source source)))
      (cl-ecase (dorg-mutate-source-kind resolved)
        (plain-org
         (or (dorg-mutate-source-buffer resolved)
             (dorg-mutate-source-file resolved)))
        (dolt
         (make-dorg-registration
          :backend (dorg-backend-dolt (dorg-mutate-source-dolt-dir resolved)
                                      (dorg-mutate-source-root-id resolved))
          :file (dorg-mutate-source-file resolved)
          :identity (dorg-mutate--source-identity resolved))))))
   (t source)))

(defun dorg-api--owned-query-buffer (file)
  "Open FILE as a DOrg-owned query buffer and return it.
The buffer is intentionally file-visiting so `dorg--classify-registration'
exposes the plain-org backend to `dorg-ql-select'.  Callers must kill it
after the scoped query expansion."
  (let ((buf (dorg--find-file-noselect file)))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (org-mode)))
    buf))

(defun dorg-api--kill-owned-query-buffer (buffer)
  "Kill DOrg-owned query BUFFER without prompting."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (set-buffer-modified-p nil))
    (kill-buffer buffer)))

(defun dorg-api--ql-select (query source)
  "Run QUERY against SOURCE, scoping any owned file query buffer to this call."
  (let* ((resolved (and source
                        (or (dorg-mutate-source-p source)
                            (and (consp source) (keywordp (car source))))
                        (dorg-mutate--resolve-source source)))
         ;; BACKEND-DERIVED lifetime (D4): every no-buffer plain-org file source
         ;; gets a DOrg-owned query buffer scoped to this call — independent of any
         ;; identity policy (the removed faithful discriminator).
         (owned (and resolved
                     (eq (dorg-mutate-source-kind resolved) 'plain-org)
                     (dorg-mutate-source-file resolved)
                     (file-readable-p (dorg-mutate-source-file resolved))
                     (not (dorg-mutate-source-buffer resolved))))
         (ql-source (if owned
                        (dorg-api--owned-query-buffer
                         (dorg-mutate-source-file resolved))
                      (dorg-api--ql-source source))))
    (unwind-protect
        (dorg-ql-select query ql-source)
      (when owned
        (dorg-api--kill-owned-query-buffer ql-source)))))

(defun dorg-api-read (form)
  "Shape-validate one public mutate FORM; return it, or signal `dorg-api-error'.
FORM is (on TARGET ACTION…) | (insert (heading …) (under …))
| (batch FORM…)."
  (pcase form
    (`(on ,_ . ,actions)
     (unless actions
       (dorg-api--raise 'invalid-op "(on …) needs an action" :form form))
     (unless (proper-list-p actions)
       (dorg-api--raise 'invalid-target
                        "(on …) actions must form a proper list" :form form))
     form)
    (`(insert (heading . ,_) (under . ,_)) form)
    (`(insert . ,_)
     (dorg-api--raise 'invalid-target
                      "invalid or retired insert-heading form" :form form))
    (`(batch . ,forms)
     (unless (proper-list-p forms)
       (dorg-api--raise 'invalid-target
                        "batch members must form a proper list" :form form))
     (mapc #'dorg-api-read forms)
     form)
    (_ (dorg-api--raise 'invalid-op "unknown mutate form" :form form))))

(defun dorg-api-normalize (form &optional source)
  "Normalize a public mutate FORM into a list of TYPED PATCH IR v2 patches.
Expands (where QUERY) targets via `dorg-ql-select' against SOURCE — the SAME
path dry-run and apply share.  Signals `dorg-api-error' on any malformed
form, value, occurrence, protected key, or document-on-heading target."
  (dorg-api-read form)
  (let ((patches
         (pcase form
           (`(batch . ,forms)
            (apply #'append (mapcar (lambda (f) (dorg-api-normalize f source)) forms)))
           (`(insert (heading . ,_) (under . ,_))
            (list (dorg-api--insert-heading->patch form source)))
           (`(on ,dsl-target . ,actions)
            (let ((hosts (dorg-api--target-hosts dsl-target source)))
              (apply #'append
                     (mapcar (lambda (host)
                               (mapcar (lambda (a) (dorg-api--action->patch host a source))
                                       actions))
                             hosts)))))))
    ;; The identity-key write-guard is a WRITE-time concern that resolves SOURCE.
    ;; Pure IR normalization is SOURCE-OPTIONAL: only run the guard when a literal
    ;; SOURCE was supplied (apply/dry-run always supply one).  An explicit nonnil
    ;; source still resolves and PROPAGATES (no catch/default) so the A2 seam and
    ;; malformed-source contracts are preserved.
    (when source
      (dorg-api--guard-identity-key-writes patches source))
    patches))

(defun dorg-api--guard-identity-key-writes (patches source)
  "Refuse a (set/delete property K) on a (:property K) binding.
K is the engine-load-bearing identity anchor — mutating it via a property op
severs identity — so it is `invalid-target', the same class as the DORG_ID
guard.  The policy is read through the single resolver
`dorg-mutate--source-identity' (an unresolvable source simply carries no
identity to guard; the resolver call itself is never caught/collapsed)."
  (let* ((policy (dorg-mutate--source-identity (dorg-mutate--resolve-source source)))
         (idkey (and (consp policy) (eq (car policy) :property) (cadr policy))))
    (when idkey
      (dolist (p patches)
        (let ((tg (plist-get p :target)))
          (when (and (consp tg) (eq (car tg) :property)
                     (memq (plist-get p :op) '(put delete))
                     (equal (dorg-api--tget tg :name) idkey))
            (dorg-api--raise 'invalid-target
                             "declared identity-key is write-protected"
                             :key idkey)))))))

;;; ─── Command envelope: execute (dry-run / apply + safety gates) ──────────

(cl-defstruct (dorg-api--mutation-intent
               (:constructor dorg-api--mutation-intent-create)
               (:copier nil))
  "Private owned storage for a public mutation intent value."
  form
  options)

(defconst dorg-api--mutation-intent-option-keys
  '(:source :base :expected-targets :allow-broad :save :message :op-id
    :return-candidate-models)
  "Closed option domain owned by a mutation intent.")

(defun dorg-api--intent-copy (value)
  "Return an owned deep copy of mutable intent VALUE.
Conses, strings, and vectors (including record-backed source descriptors) are
copied recursively.  Buffers and other external identity handles are retained."
  (cond
   ((consp value)
    (cons (dorg-api--intent-copy (car value))
          (dorg-api--intent-copy (cdr value))))
   ((stringp value) (copy-sequence value))
   ((vectorp value)
    (let ((copy (copy-sequence value))
          (index 0))
      (while (< index (length copy))
        (aset copy index (dorg-api--intent-copy (aref copy index)))
        (setq index (1+ index)))
      copy))
   (t value)))

(defun dorg-api--intent-options (options allowed context)
  "Validate OPTIONS against ALLOWED and return an owned copy.
CONTEXT names the public boundary in typed refusal messages.  The option list
must be a proper, even plist with keyword keys and no duplicates."
  (unless (and (proper-list-p options) (cl-evenp (length options)))
    (dorg-api--raise 'malformed
                     (format "%s options must be a proper even plist" context)
                     :options options))
  (let (seen)
    (cl-loop for (key _value) on options by #'cddr
             do
             (unless (keywordp key)
               (dorg-api--raise 'malformed
                                (format "%s option names must be keywords" context)
                                :option key))
             (unless (memq key allowed)
               (dorg-api--raise 'malformed
                                (format "%s does not accept option %S" context key)
                                :option key))
             (when (memq key seen)
               (dorg-api--raise 'malformed
                                (format "%s option %S is duplicated" context key)
                                :option key))
             (push key seen)))
  (dorg-api--intent-copy options))

(defun dorg-api-mutation-intent-p (value)
  "Return non-nil when VALUE is a first-class mutation intent."
  (dorg-api--mutation-intent-p value))

(defun dorg-api-make-mutation-intent (form &rest options)
  "Return an immutable mutation intent owning public FORM and OPTIONS.
The closed option domain is `:source', `:base', `:expected-targets',
`:allow-broad', `:save', `:message', `:op-id', and
`:return-candidate-models' (tracked-regression).  Construction validates
shape and ownership only: source descriptors and targets remain unresolved
until each execution."
  (let ((owned-form (dorg-api--intent-copy form))
        (owned-options
         (dorg-api--intent-options
          options dorg-api--mutation-intent-option-keys
          "mutation intent")))
    (dorg-api-read owned-form)
    (dorg-api--mutation-intent-create :form owned-form
                                      :options owned-options)))

(defun dorg-api--require-mutation-intent (intent)
  "Return INTENT or signal a typed refusal when it is not an intent value."
  (unless (dorg-api--mutation-intent-p intent)
    (dorg-api--raise 'malformed "expected a mutation intent"
                     :value intent))
  intent)

(defun dorg-api-mutation-intent-form (intent)
  "Return a detached copy of INTENT's public mutation form."
  (dorg-api--intent-copy
   (dorg-api--mutation-intent-form
    (dorg-api--require-mutation-intent intent))))

(defun dorg-api-mutation-intent-options (intent)
  "Return a detached copy of INTENT's option plist.
Presence is retained independently of value, including explicitly supplied nil."
  (dorg-api--intent-copy
   (dorg-api--mutation-intent-options
    (dorg-api--require-mutation-intent intent))))

(defun dorg-api-mutation-intent-with (intent &rest replacements)
  "Return a new INTENT with functional option REPLACEMENTS.
The original remains unchanged.  REPLACEMENTS use the same closed option
domain as `dorg-api-make-mutation-intent'."
  (dorg-api--require-mutation-intent intent)
  (let ((options (dorg-api--intent-copy
                  (dorg-api--mutation-intent-options intent)))
        (updates
         (dorg-api--intent-options
          replacements dorg-api--mutation-intent-option-keys
          "mutation intent update")))
    (cl-loop for (key value) on updates by #'cddr
             do (setq options (plist-put options key value)))
    (apply #'dorg-api-make-mutation-intent
           (dorg-api--mutation-intent-form intent)
           options)))

(defun dorg-api--raw-intent-options (options)
  "Return the intent-owned subset of legacy raw execute OPTIONS.
Unknown raw-form keywords retain the historical ignored behavior.  Repeated
known keys retain the historical first-value behavior while the constructed
intent itself is canonical and duplicate-free."
  (unless (and (proper-list-p options) (cl-evenp (length options)))
    (dorg-api--raise 'malformed
                     "execute options must be a proper even plist"
                     :options options))
  (let (out)
    (dolist (key dorg-api--mutation-intent-option-keys out)
      (when (plist-member options key)
        (setq out (append out (list key (plist-get options key))))))))

(defun dorg-api--query-scoped-p (form)
  "Return non-nil when FORM (or a member) is a (on (where …) …) form."
  (pcase form
    (`(on (where . ,_) . ,_) t)
    (`(batch . ,fs) (cl-some #'dorg-api--query-scoped-p fs))
    (_ nil)))

(defun dorg-api--selector-bounded-p (form)
  "Non-nil when FORM's target selector ASSERTS cardinality <= 1.
The semantic gate rule (docs/mutate.org, docs/api.org \"Grammar
resolution\"): an apply whose selector is bounded — a direct id/key/document
target, an insert, or a (where SEL) whose SEL top form is (key …) or
(one …) — is EXEMPT from the :expected-targets ceremony, since it resolves
to at most one known target.  A batch is bounded only when every member
is.  An unbounded (where …) (e.g. a bare predicate or pipe) is NOT."
  (pcase form
    (`(on (where ,sel) . ,_)
     (and (consp sel) (memq (car sel) '(key one))))
    (`(on (where . ,_) . ,_) nil)
    (`(on . ,_) t)             ; direct id/key/document — not query-scoped
    (`(insert (heading . ,_) (under . ,_)) t)
    (`(batch . ,fs) (cl-every #'dorg-api--selector-bounded-p fs))
    (_ nil)))

(defun dorg-api--patch-identity (patch)
  "Return the identity a PATCH targets: an integer id, extern host, or nil.
Property/keyword/body patches carry a `:host'; heading/subtree patches carry
direct `:id' or inline `:extern'.  An extern host projects to
(:extern :key K :value V)."
  (let* ((tg (plist-get patch :target))
         (host (dorg-api--tget tg :host)))
    (cond
     ((and (consp tg) (memq (car tg) '(:heading :subtree))
           (eq (cadr tg) :extern))
      (list :extern :key (dorg-api--tget tg :key) :value (dorg-api--tget tg :value)))
     ((and (consp host) (eq (car host) :heading) (eq (cadr host) :extern))
      (list :extern :key (dorg-api--tget host :key) :value (dorg-api--tget host :value)))
     ((and (consp host) (eq (car host) :heading))
      (dorg-api--tget host :id))
     ((dorg-api--tget tg :id) (dorg-api--tget tg :id))
     (t nil))))

(defun dorg-api--patch-target-ids (patches)
  "Return the canonical, token-sorted, duplicate-free identity set PATCHES touch.
Each identity is an integer heading id or an extern host (:extern …); the set is
ordered by canonical token so mixed id+extern sets compare deterministically."
  (dorg-api--sort-identities
   (dorg-api--dedup-identities
    (delq nil (mapcar #'dorg-api--patch-identity patches)))))

(defun dorg-api--reject-extension-inputs (options)
  "Fail closed when OPTIONS supplies the retired `:extension-inputs' key.
The per-execution caller-payload channel is RETIRED (tracked-regression): index
projectors run automatically from the DOrg-owned candidate-state context and
accept no caller payload.  Rejection is PRESENCE-based (`plist-member'): an
explicitly supplied key — even `:extension-inputs nil' — is a superseded API and
fails closed with typed `unsupported' BEFORE any storage is touched.  An omitted
key is a no-op."
  (when (plist-member options :extension-inputs)
    (dorg-api--raise
     'unsupported
     ":extension-inputs is retired; index projectors run automatically from \
candidate state and accept no caller payload"
     :detail 'extension-inputs-retired)))

(defun dorg-api--execute-intent (intent mode)
  "Execute immutable INTENT in MODE.
MODE must be exactly `dry-run' or `apply' (neither/both -> `mode-error').
Query-scoped apply requires :EXPECTED-TARGETS, compared against the re-expanded
target set by set membership: order is ignored and duplicate expected ids are
stably de-duplicated before comparison, so only a real membership difference is
`target-set-changed'.  Honors :BASE, :ALLOW-BROAD, :SAVE, :MESSAGE, and
`dorg-api-broad-write-threshold' from INTENT.  Its SOURCE descriptor remains
unresolved until this call.  Signals `dorg-api-error' on any gate/validation
failure.  This is the single execution core shared by raw forms and first-class
intent values.

The retired `:extension-inputs' caller-payload channel does not reach this core:
it fails closed at the public `dorg-api-execute' boundary (tracked-regression).  Index
projectors run automatically from the DOrg-owned candidate-state context and
accept no caller payload.  See docs/api.org and docs/mutate.org."
  (dorg-api--require-mutation-intent intent)
  (unless (memq mode '(dry-run apply))
    (dorg-api--raise 'mode-error "exactly one of dry-run / apply is required" :mode mode))
  ;; D4: resolve the public SOURCE descriptor ONCE and validate its identity
  ;; policy up front — UNCAUGHT and load-bearing, so a malformed `:identity' or
  ;; an underivable source kind signals here, before any normalization or engine
  ;; work (never caught, defaulted, or dummied).  Resolution is UNCONDITIONAL: a
  ;; nil source in public execute means the current buffer and must resolve to a
  ;; real backing binding — the source-agnostic literal-nil path is reserved for
  ;; a DIRECT `dorg-api-normalize' call, never execute.  The SAME resolved source
  ;; is then threaded through normalize/target projection, the expected-target
  ;; comparison, and the engine plan/apply; `dorg-mutate--resolve-source' is
  ;; idempotent on a resolved struct, so every downstream re-resolution returns
  ;; this same binding.
  (let* ((form (dorg-api--mutation-intent-form intent))
         (options (dorg-api--mutation-intent-options intent))
         (expected-targets (plist-get options :expected-targets))
         (base (plist-get options :base))
         (allow-broad (plist-get options :allow-broad))
         (save (plist-get options :save))
         (message (plist-get options :message))
         (op-id (plist-get options :op-id))
         (source (let ((s (dorg-mutate--resolve-source
                           (plist-get options :source))))
                   (dorg-mutate--source-identity s)
                   s))
         (patches (dorg-api-normalize form source))
         (targets (dorg-api--patch-target-ids patches))
         ;; A bounded selector (direct id/key/document, or bounded where)
         ;; asserts cardinality <= 1, so it is EXEMPT from the query-scoped
         ;; :expected-targets ceremony even though it rides (where …).
         (query-scoped (and (dorg-api--query-scoped-p form)
                            (not (dorg-api--selector-bounded-p form)))))
    ;; ── Query-scoped apply safety gates ─────────────────────────────────
    (when (and (eq mode 'apply) query-scoped)
      (unless (plist-member options :expected-targets)
        (dorg-api--raise 'expected-targets-required
                         "query-scoped apply requires :expected-targets" :targets targets))
      (unless (equal (mapcar #'dorg-api-target-token targets)
                     (mapcar #'dorg-api-target-token
                             (dorg-api--sort-identities
                              (dorg-api--dedup-identities
                               (copy-sequence expected-targets)))))
        (dorg-api--raise 'target-set-changed
                         "resolved target set differs from :expected-targets"
                         :targets targets :expected expected-targets)))
    (when (and (eq mode 'apply) (not allow-broad)
               (> (length targets) dorg-api-broad-write-threshold))
      (dorg-api--raise 'broad-write-refused
                       (format "apply over %d targets exceeds threshold %d; pass --allow-broad"
                               (length targets) dorg-api-broad-write-threshold)
                       :count (length targets)))
    ;; ── D5 public freshness token + engine call + canonical reshape ─────
    ;; `:base' is the PUBLIC pre-write binding freshness token, BACKEND-AWARE:
    ;;  - plain-org: the public source-state token from
    ;;    `dorg-mutate--source-public-base' — the live buffer token
    ;;    (:kind buffer :buffer NAME :file F :tick TICK :fingerprint FP) when a
    ;;    buffer visits the source, else the file token
    ;;    (:kind file :file F :mtime M :fingerprint FP).  The `:fingerprint' is an
    ;;    exact raw-content hash carried IN ADDITION to the stat counter, so the
    ;;    command-layer CAS also catches an equal-size/equal-mtime external
    ;;    replacement across dry-run -> apply (a stat-only token cannot see it).  A
    ;;    supplied `:base' is validated HERE (catching that replacement and an
    ;;    unsaved buffer edit between dry-run and apply) and is NEVER forwarded to
    ;;    the engine as `:base-revision'.
    ;;  - Dolt: the engine's own commit token.  The supplied `:base' is forwarded
    ;;    ONLY as `:base-commit' so the native Dolt HEAD CAS runs; the response
    ;;    `:base' is the engine commit (base-commit, or the plan's head-commit on
    ;;    a fresh dry-run).  No `:base-revision' is involved for Dolt.
    (let* ((kind (dorg-mutate-source-kind source))
           (pub-base (dorg-mutate--source-public-base source)))
      (when (and (eq mode 'apply) base (eq kind 'plain-org)
                 (not (equal base pub-base)))
        (dorg-api--raise 'stale "plain-org source changed since :base was captured"
                         :detail 'base-revision :base base :current pub-base))
      ;; `:op-id' is the PUBLIC exactly-once idempotency key (§2.6): the caller
      ;; generates it ONCE per logical mutation and reuses it verbatim across
      ;; retries/restarts.  DOrg binds it to an INTENT DIGEST computed from the
      ;; immutable typed patches + the store's stable identity (never the client
      ;; path), so it is identical across re-plans and across source aliases; a
      ;; replay carrying the same op-id but a different intent is rejected.  Both
      ;; ride into the engine ONLY for a Dolt apply (the restart-safe evidence
      ;; log is Dolt-native); a plain-org op-id is echoed but not enforced.
      (let* ((intent-digest (and op-id (eq kind 'dolt) (eq mode 'apply)
                                 (dorg-mutate--intent-digest patches source)))
             (engine-opts (append (when (and base (eq kind 'dolt))
                                    (list :base-commit base))
                                  (when (and op-id (eq kind 'dolt))
                                    (list :op-id op-id :intent-digest intent-digest))
                                  ;; Wire v1 (tracked-regression): the candidate-model
                                  ;; receipt is a Dolt-apply capability — models
                                  ;; serialize from committed readback, which a
                                  ;; dry-run or plain-org apply cannot produce.
                                  ;; Elsewhere the option is inert; a consumer
                                  ;; publishing verified success fails closed on
                                  ;; the missing receipt, never silently.
                                  (when (and (plist-get options
                                                        :return-candidate-models)
                                             (eq kind 'dolt) (eq mode 'apply))
                                    (list :return-candidate-models t))
                                  (when (plist-member options :save)
                                    (list :save save))
                                  (when (plist-member options :message)
                                    (list :message message))))
             (res (condition-case err
                      (if (eq mode 'dry-run)
                          (apply #'dorg-mutate--plan-patches source patches nil engine-opts)
                        (apply #'dorg-mutate--apply-patches source patches nil engine-opts))
                    (dorg-mutate-error
                     (let* ((data (dorg-mutate-error-data err))
                            (target (plist-get data :target)))
                       (if (and (eq 'invalid-target (dorg-mutate-error-reason err))
                                (eq 'ambiguous (plist-get data :detail))
                                (and (consp target) (eq (car target) :extern)))
                           (dorg-api--raise
                            'identity-violation
                            "external identity key matched more than one heading"
                            :target target :detail 'ambiguous)
                         (signal (car err) (cdr err)))))))
             (resp-base (if (eq kind 'plain-org)
                            pub-base
                          (or (plist-get res :base-commit)
                              (plist-get res :head-commit))))
             ;; D5 projections eligibility: a per-target before/after preview is
             ;; produced for a SUPPORTED, READABLE plain-org DRY-RUN.  When
             ;; eligible the field is ALWAYS PRESENT (nil for a zero-match plan);
             ;; it is OMITTED only for an inapplicable backend/unsupported/apply.
             (proj-eligible (and (eq mode 'dry-run)
                                 (eq kind 'plain-org)
                                 (if (plist-member res :supported)
                                     (plist-get res :supported) t)
                                 (dorg-mutate--plain-org-source-readable-p source)))
             (projections (and proj-eligible
                               (dorg-api--projections source patches targets))))
        (dorg-api--reshape res mode form patches targets
                           resp-base proj-eligible projections op-id)))))

(defun dorg-api-execute (form-or-intent &rest options)
  "Execute FORM-OR-INTENT under the public mutate command envelope.
OPTIONS carries the mode and, for a raw form, the established execute options.
The retired `:extension-inputs' key fails closed by presence on either route
(tracked-regression).  A first-class mutation intent accepts exactly `:mode';
derive any other variation with `dorg-api-mutation-intent-with'.  Both routes
converge on `dorg-api--execute-intent'.  The response retains the public form in
`:intent'; the opaque intent value is never serialized."
  (if (dorg-api--mutation-intent-p form-or-intent)
      (let* ((execute-options
              (dorg-api--intent-options options '(:mode)
                                        "mutation intent execution"))
             (mode (plist-get execute-options :mode)))
        (dorg-api--execute-intent form-or-intent mode))
    ;; Preserve the historical error priority: invalid/missing mode refuses
    ;; before raw form or source work.
    (let ((mode (plist-get options :mode)))
      (unless (memq mode '(dry-run apply))
        (dorg-api--raise 'mode-error
                         "exactly one of dry-run / apply is required"
                         :mode mode))
      ;; The retired :extension-inputs key fails closed by PRESENCE (even an
      ;; explicit nil), before any raw-form or source work (tracked-regression).
      (dorg-api--reject-extension-inputs options)
      (dorg-api--execute-intent
       (apply #'dorg-api-make-mutation-intent
              form-or-intent (dorg-api--raw-intent-options options))
       mode))))

(defun dorg-api--projections (source patches targets)
  "Build the ordered D5 `:projections' list for a plain-org dry-run.
One entry per TARGET in canonical token order (TARGETS is already canonically
sorted), each the exact D5 plist
\(:target TOKEN :change-kind KIND :before STRING :after STRING :provision P) in
the EXACT D5 field order.  The per-host before/after regions come from
`dorg-mutate--plain-org-projections' (computed on an isolated copy); a target
identity IS its executor host, so they correlate by `equal'."
  (let ((by-host (dorg-mutate--plain-org-projections source patches)))
    (mapcar (lambda (id)
              (let ((pl (cdr (assoc id by-host))))
                (list :target (dorg-api-target-token id)
                      :change-kind (plist-get pl :change-kind)
                      :before (plist-get pl :before)
                      :after (plist-get pl :after)
                      :provision (plist-get pl :provision))))
            targets)))

(defun dorg-api--reshape (res mode form patches targets base proj-eligible
                          projections &optional op-id)
  "Reshape an engine plan/result RES into the canonical command response.
BASE is the D5 public freshness token.  When PROJ-ELIGIBLE, `:projections' is
PRESENT (PROJECTIONS, possibly nil for a zero-match plan); otherwise the field is
omitted (inapplicable backend/unsupported/apply).

OP-ID is the caller-supplied exactly-once idempotency key (§2.6); when non-nil it
is ECHOED as `:op-id' on the response (present-only, so an omitted op-id keeps
the pre-change byte shape), alongside the engine's `:idempotent' flag when the
apply reconciled to an already-committed generation instead of re-applying.

The `:extensions' index-projector SUMMARY alist is surfaced whenever an index
projector actually ran (RES carries a non-nil `:extensions'), and omitted
otherwise — dirty scope is DOrg-derived from the single projector result, so no
caller opt-in is involved (tracked-regression).  The engine-internal `:ext-sql'/
`:db-ops' (derived SQL) are NEVER surfaced here; `:invalidation' passes through
\(apply carries an advancing head token, a dry-run carries a preview with only
the STABLE `:roots' + `:ext-dirty' — the command layer compares only those
stable fields, not the head)."
  (append
   (list :ok t :command 'mutate :mode mode
         ;; Keep the opaque value private and preserve its ownership boundary:
         ;; the public wire form is a detached copy, never the stored object.
         :intent (list (dorg-api--intent-copy form))
         :patches patches
         :targets targets
         :base base
         :affected (list :created (plist-get res :created)
                         :changed (plist-get res :changed)
                         :deleted (plist-get res :deleted))
         :supported (if (plist-member res :supported) (plist-get res :supported) t)
         :unsupported (plist-get res :unsupported)
         :base-commit (plist-get res :base-commit)
         :base-revision (plist-get res :base-revision)
         :new-token (or (plist-get res :revision) (plist-get res :commit)))
   (when proj-eligible (list :projections projections))
   (when (plist-get res :extensions)
     (list :extensions (plist-get res :extensions)))
   (when (plist-member res :invalidation)
     (list :invalidation (plist-get res :invalidation)))
   ;; Versioned canonical postimage receipt (tracked-regression): one entry per
   ;; affected owner partition on a committed Dolt apply, and an idempotent
   ;; replay RE-EMITS the receipt stored with its evidence row.  Surfaced
   ;; only when non-empty — dry-run, plain-org, zero-effect, identity-less,
   ;; and legacy-evidence results carry none, preserving the pre-change
   ;; shape.
   (when (plist-get res :postimages)
     (list :postimages (plist-get res :postimages)))
   ;; Wire v1 candidate models (tracked-regression AC4): present only when the
   ;; caller requested them AND a committed (or settled) Dolt apply produced
   ;; them from its verified readback.
   (when (plist-get res :candidate-models)
     (list :candidate-models (plist-get res :candidate-models)))
   ;; A Dolt apply through the per-store gate carries phase `:timings'
   ;; (gate-wait/gate-hold/plan/sql/commit); surface it when present so the
   ;; public result exposes the boundary latency (A11/A14).  Dry-run and
   ;; plain-org results have none, so the pre-change shape is preserved.
   (when (plist-member res :timings)
     (list :timings (plist-get res :timings)))
   ;; Exactly-once public surface (A14/§4): echo the caller's op-id on every
   ;; result, and flag an idempotent replay reconciliation.  Present-only, so a
   ;; caller that supplies no op-id sees the byte-identical pre-change shape.
   (when op-id (list :op-id op-id))
   (when (plist-get res :idempotent) (list :idempotent t))))

(provide 'dorg-api)
;;; dorg-api.el ends here
