;;; dorg-envelope.el --- Command-boundary JSON envelopes -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Shared structured response helpers for public command boundaries.

;;; Code:

(require 'cl-lib)

(defun dorg-envelope-success (data)
  "Return the public success envelope around DATA."
  (list :ok t :data data :error nil))

(defun dorg-envelope-error (reason message &optional details)
  "Return the public error envelope for REASON, MESSAGE, and DETAILS."
  (list :ok :json-false :data nil
        :error (append (list :reason reason :message message)
                       (when details (list :details details)))))

(defun dorg-envelope--key (key)
  "Return KEY as a public kebab-case JSON key symbol."
  (let* ((name (cond
                ((keywordp key) (substring (symbol-name key) 1))
                ((symbolp key) (symbol-name key))
                ((stringp key) key)
                (t (format "%s" key))))
         (kebab (replace-regexp-in-string "_" "-" name)))
    (intern kebab)))

(defun dorg-envelope--plist-p (value)
  "Return non-nil when VALUE is a keyword-keyed plist."
  (and (consp value)
       (keywordp (car value))
       (cl-evenp (length value))))

(defun dorg-envelope--alist-p (value)
  "Return non-nil when VALUE is an alist."
  (and (listp value)
       (cl-every #'consp value)))

(defun dorg-envelope--serialize-plist (plist)
  "Serialize keyword-keyed PLIST as a JSON object alist."
  (let (out)
    (while plist
      (let ((k (pop plist))
            (v (pop plist)))
        (push (cons (dorg-envelope--key k)
                    (dorg-envelope-serialize v))
              out)))
    (nreverse out)))

(defun dorg-envelope-serialize (value)
  "Return VALUE recursively normalized for public JSON encoding.
Object keys are kebab-case symbols.  Ordered non-object lists become
vectors so public arrays stay arrays under `json-encode'."
  (cond
   ((or (null value)
        (eq value t)
        (eq value :json-false)
        (eq value :json-null)
        (stringp value)
        (numberp value))
    value)
   ((vectorp value)
    (vconcat (mapcar #'dorg-envelope-serialize value)))
   ((dorg-envelope--plist-p value)
    (dorg-envelope--serialize-plist value))
   ((dorg-envelope--alist-p value)
    (mapcar (lambda (pair)
              (cons (dorg-envelope--key (car pair))
                    (dorg-envelope-serialize (cdr pair))))
            value))
   ((listp value)
    (vconcat (mapcar #'dorg-envelope-serialize value)))
   (t value)))

(defun dorg-envelope-success-json (data)
  "Return DATA wrapped and normalized for public JSON encoding."
  (dorg-envelope-serialize (dorg-envelope-success data)))

(defun dorg-envelope-error-json (reason message &optional details)
  "Return an error envelope normalized for public JSON encoding."
  (dorg-envelope-serialize (dorg-envelope-error reason message details)))

(provide 'dorg-envelope)
;;; dorg-envelope.el ends here
