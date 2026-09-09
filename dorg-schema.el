;;; dorg-schema.el --- Dolt database schema for dorg -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is not part of GNU Emacs.

;;; Commentary:

;; SQL schema constants for dorg.  Defines the relational mapping
;; of org-mode file structure to Dolt tables.
;;
;; The schema uses a polymorphic heading_items table as the ordering
;; backbone — all content under a heading (text, keywords, planning,
;; drawers) is stored as typed items with a binary sort key that
;; preserves exact interleaving order for round-trip fidelity.
;;
;; Multi-file support: each heading carries a `root_id' pointing at
;; its tree's level-0 root.  The root heading satisfies id = root_id.
;; The `dorg_files' table persists store-relative path → root_id for lookup.

;;; Code:

(defconst dorg-schema-version 6
  "Current schema version.")

;;; ─── Schema-version skew ─────────────────────────────────────────────
;;
;; A dorg installation and the live Dolt repo it opens can drift in
;; schema version.  The hazard is asymmetric: a repo NEWER than the dorg
;; reading it cannot be made compatible by migrating (dorg never
;; downgrades), and auto-migrating an OLDER repo UP is a one-way upgrade
;; that locks out every co-installed dorg still pinned below the new
;; version (e.g. a CLI's vendored `lib/dorg' submodule).  These errors
;; and the `dorg-schema-auto-migrate' policy make the skew fail early and
;; actionably instead of mid-write, and keep the upgrade from being
;; silent.  See `dorg-repo-check-schema-compat'.

(define-error 'dorg-schema-incompatible
  "dorg schema version is incompatible with this dorg")

(define-error 'dorg-schema-repo-newer
  "dorg repo schema is newer than this dorg supports"
  'dorg-schema-incompatible)

(define-error 'dorg-schema-migration-required
  "dorg repo needs a schema migration that is not enabled"
  'dorg-schema-incompatible)

(defcustom dorg-schema-auto-migrate t
  "Whether opening an OLDER Dolt repo auto-migrates it to the current schema.

When non-nil (the default), `dorg-repo-ensure-schema' upgrades a repo
whose recorded schema version is below `dorg-schema-version', emitting a
`display-warning' first so the one-way upgrade is never silent.  The
upgrade LOCKS OUT any older co-installed dorg (e.g. a CLI's vendored
`lib/dorg') from writing the repo until it too is upgraded.

When nil, such a repo is left untouched and `dorg-repo-ensure-schema'
signals `dorg-schema-migration-required'.  Shared, multi-installation
deployments — a fleet of CLIs writing one tracker — should set this nil
so no single process silently upgrades the shared repo out from under the
others; migrate explicitly with `dorg-migrate-schema' once every
installation is current."
  :type 'boolean
  :group 'dorg)

(defcustom dorg-schema-diagnostics-show-source-path nil
  "When non-nil, schema version-skew diagnostics append this dorg's source path.
The path comes from `locate-library' and pinpoints WHICH installation is
stale (e.g. a CLI's vendored `lib/dorg' copy versus an interactive Emacs
checkout).  The default is nil so normal public warnings and reproducible
logs are not littered with surprising local absolute checkout paths;
operators who need to identify the exact file can opt in."
  :type 'boolean
  :group 'dorg)

(defun dorg-schema--source-location (&optional with-path)
  "Describe this dorg's schema version for version-skew diagnostics.
Returns a path-free identifier (\"schema vN\") by default, so public
warnings and logs do not leak a local absolute source path.  When
WITH-PATH is non-nil — or `dorg-schema-diagnostics-show-source-path' is
set — also append the defining file (`locate-library') as operator detail,
identifying WHICH dorg installation is stale."
  (if (or with-path dorg-schema-diagnostics-show-source-path)
      (format "schema v%d (%s)"
              dorg-schema-version
              (or (locate-library "dorg-schema") "unknown location"))
    (format "schema v%d" dorg-schema-version)))

(defconst dorg-schema-ddl
  `("CREATE TABLE dorg_meta (
  `key`   VARCHAR(100) PRIMARY KEY,
  value   TEXT
)"
    "CREATE TABLE headings (
  id              BIGINT PRIMARY KEY,
  root_id         BIGINT NOT NULL,
  parent_id       BIGINT NULL,
  sort_key        VARBINARY(64) NOT NULL,
  level           INT NOT NULL,
  todo_keyword    VARCHAR(32) NULL,
  priority        CHAR(1) NULL,
  commented       BOOLEAN NOT NULL DEFAULT FALSE,
  title           TEXT NULL,
  post_blank      INT NOT NULL DEFAULT 0,
  items_hash      CHAR(32) NULL,
  FOREIGN KEY (parent_id) REFERENCES headings(id) ON DELETE CASCADE,
  FOREIGN KEY (root_id)   REFERENCES headings(id) ON DELETE CASCADE,
  UNIQUE KEY uq_heading_sibling_order (parent_id, sort_key),
  INDEX idx_headings_root (root_id),
  INDEX idx_headings_root_parent (root_id, parent_id)
)"
    "CREATE TABLE dorg_files (
  root_id         BIGINT PRIMARY KEY,
  relative_path   VARCHAR(1024) NOT NULL,
  UNIQUE KEY uq_dorg_files_relative_path (relative_path),
  FOREIGN KEY (root_id) REFERENCES headings(id) ON DELETE CASCADE
)"
    "CREATE TABLE heading_tags (
  heading_id      BIGINT NOT NULL,
  ordinal         INT NOT NULL,
  tag             VARCHAR(255) NOT NULL,
  PRIMARY KEY (heading_id, ordinal),
  UNIQUE KEY uq_heading_tag_value (heading_id, tag),
  FOREIGN KEY (heading_id) REFERENCES headings(id) ON DELETE CASCADE
)"
    "CREATE TABLE heading_items (
  id              BIGINT PRIMARY KEY,
  heading_id      BIGINT NOT NULL,
  sort_key        VARBINARY(64) NOT NULL,
  kind            VARCHAR(32) NOT NULL,
  post_blank      INT NOT NULL DEFAULT 0,
  FOREIGN KEY (heading_id) REFERENCES headings(id) ON DELETE CASCADE,
  UNIQUE KEY uq_heading_item_order (heading_id, sort_key)
)"
    "CREATE TABLE text_items (
  item_id         BIGINT PRIMARY KEY,
  text_raw        LONGTEXT NOT NULL,
  FOREIGN KEY (item_id) REFERENCES heading_items(id) ON DELETE CASCADE
)"
    "CREATE TABLE keyword_items (
  item_id         BIGINT PRIMARY KEY,
  keyword         VARCHAR(255) NOT NULL,
  value_raw       TEXT NOT NULL,
  FOREIGN KEY (item_id) REFERENCES heading_items(id) ON DELETE CASCADE
)"
    "CREATE TABLE planning_items (
  item_id         BIGINT PRIMARY KEY,
  scheduled_raw   VARCHAR(255) NULL,
  deadline_raw    VARCHAR(255) NULL,
  closed_raw      VARCHAR(255) NULL,
  FOREIGN KEY (item_id) REFERENCES heading_items(id) ON DELETE CASCADE
)"
    "CREATE TABLE drawer_items (
  item_id         BIGINT PRIMARY KEY,
  name            VARCHAR(255) NOT NULL,
  FOREIGN KEY (item_id) REFERENCES heading_items(id) ON DELETE CASCADE
)"
    "CREATE TABLE drawer_lines (
  id              BIGINT PRIMARY KEY AUTO_INCREMENT,
  drawer_item_id  BIGINT NOT NULL,
  ordinal         INT NOT NULL,
  line_raw        TEXT NOT NULL,
  FOREIGN KEY (drawer_item_id) REFERENCES drawer_items(item_id) ON DELETE CASCADE,
  UNIQUE KEY uq_drawer_line_ordinal (drawer_item_id, ordinal)
)"
    "CREATE TABLE property_entries (
  id              BIGINT PRIMARY KEY AUTO_INCREMENT,
  drawer_item_id  BIGINT NOT NULL,
  ordinal         INT NOT NULL,
  prop_key        VARCHAR(255) NOT NULL,
  prop_value      TEXT NOT NULL,
  raw_line        TEXT NOT NULL,
  FOREIGN KEY (drawer_item_id) REFERENCES drawer_items(item_id) ON DELETE CASCADE,
  UNIQUE KEY uq_property_entry_ordinal (drawer_item_id, ordinal)
)"
    "CREATE VIEW heading_properties AS
SELECT pe.drawer_item_id, di.item_id, hi.heading_id,
       pe.ordinal, pe.prop_key, pe.prop_value
FROM property_entries pe
JOIN drawer_items di ON pe.drawer_item_id = di.item_id
JOIN heading_items hi ON di.item_id = hi.id
WHERE di.name = 'PROPERTIES'"
    "INSERT INTO dorg_meta (`key`, value) VALUES ('schema_version', '6')")
  "SQL DDL statements for fresh dorg repos (always reflects latest schema version).
A list of individual SQL statements executed via `dorg-sql-batch'.
Existing repos are migrated incrementally via `dorg-schema-migrations'.")

(defconst dorg-schema-migrations nil
  "Alist of (FROM-VERSION . STATEMENTS) for incremental schema migrations.
Each entry migrates from FROM-VERSION to FROM-VERSION+1.
STATEMENTS is a list of SQL strings, or a repository-owned function that
returns them when the migration depends on bounded store context.  The first
public release starts at schema v6, so no pre-release migration entries ship;
this empty runner remains for future v6 -> v7+ migrations.")

(provide 'dorg-schema)
;;; dorg-schema.el ends here
