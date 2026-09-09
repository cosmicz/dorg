;;; dorg-find.el --- Heading/property lookup for dorg -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Stable by-id/by-property lookup facade.  Clients can ask for
;; headings by their `DORG_ID' or by an arbitrary property key/value
;; without knowing whether the backing store is a plain org file or a
;; Dolt repository.
;;
;; Public entry points:
;;
;;   `dorg-find-heading-by-id'         — locate a heading by DORG_ID.
;;   `dorg-find-headings-by-property'  — locate headings by property.
;;
;; All results are returned as a uniform list of plists with the keys
;; `:heading-id', `:marker', `:title', and `:properties'.  Any field
;; may be nil when the backend cannot supply it (for example plain-org
;; headings without a `:DORG_ID:' property report `:heading-id' nil).
;; Markers are live markers when produced from an in-memory buffer;
;; Dolt SELECT results carry nil markers since the heading lives in the
;; database.
;;
;; Both entry points now route through the `dorg-ql' core: by-id maps to
;; the `(id ID)' query and by-property maps to `(property KEY VALUE)'.
;; The result shape and backend-dispatch behavior are unchanged; the
;; dependency points one way only (`dorg-find' -> `dorg-ql'), so the
;; query core never depends back on this facade.

;;; Code:

(require 'dorg-ql)

(defun dorg-find-heading-by-id (id &optional buffer)
  "Locate the heading whose `DORG_ID' is ID in BUFFER's storage.
Returns a list with at most one result plist (see commentary).
BUFFER defaults to the current buffer and must be registered.

This entry point is DORG_ID-only.  Use
`dorg-find-headings-by-property' to look up by a custom property
such as a bead/extension id."
  (dorg-ql-select (list 'id id) buffer))

(defun dorg-find-headings-by-property (key value &optional buffer)
  "Locate all headings with property KEY = VALUE in BUFFER's storage.
Returns a list of result plists (see commentary).  BUFFER
defaults to the current buffer and must be registered.

KEY is matched verbatim against org property keys (plain-org)
or `heading_properties.prop_key' (Dolt).  VALUE matches the
property value exactly."
  (dorg-ql-select (list 'property key value) buffer))

(provide 'dorg-find)
;;; dorg-find.el ends here
