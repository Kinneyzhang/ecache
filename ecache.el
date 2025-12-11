;;; ecache.el --- Universal cache system for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Kinney Zhang
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: cache, storage, performance
;; URL: https://github.com/Kinneyzhang/ecache

;; Note: Database cache backend requires Emacs 29.1+ with SQLite support.
;; All other features work with Emacs 26.1+.

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ecache is a universal caching system for Emacs, inspired by browser
;; localStorage/sessionStorage and other language caching libraries.
;;
;; Features:
;; - Multiple storage backends: memory, file, and database (SQLite)
;; - Buffer-local and global memory caches
;; - Flexible cache invalidation strategies (idle, time-based, hook-based)
;; - Simple key-value interface
;; - Automatic serialization/deserialization
;; - TTL (Time To Live) support
;; - LRU (Least Recently Used) eviction policy
;;
;; Usage:
;;
;;   ;; Memory cache (global)
;;   (ecache-set "my-key" "my-value")
;;   (ecache-get "my-key")
;;
;;   ;; File-based cache
;;   (ecache-set "my-key" "my-value" :backend 'file)
;;   (ecache-get "my-key" :backend 'file)
;;
;;   ;; Database cache
;;   (ecache-set "my-key" "my-value" :backend 'database)
;;   (ecache-get "my-key" :backend 'database)
;;
;;   ;; Cache with TTL (expires in 60 seconds)
;;   (ecache-set "my-key" "my-value" :ttl 60)
;;
;;   ;; Buffer-local cache
;;   (ecache-set-buffer-local "my-key" "my-value")
;;   (ecache-get-buffer-local "my-key")

;;; Code:

(require 'cl-lib)

;;; Customization

(defgroup ecache nil
  "Universal cache system for Emacs."
  :group 'convenience
  :prefix "ecache-")

(defcustom ecache-directory (expand-file-name "ecache" user-emacs-directory)
  "Directory for file-based cache storage."
  :type 'directory
  :group 'ecache)

(defcustom ecache-database-file (expand-file-name "ecache.db" ecache-directory)
  "Path to SQLite database file for cache storage."
  :type 'file
  :group 'ecache)

(defcustom ecache-default-ttl nil
  "Default TTL (time to live) in seconds for cache entries.
nil means no expiration by default."
  :type '(choice (const :tag "No expiration" nil)
                 (integer :tag "Seconds"))
  :group 'ecache)

(defcustom ecache-max-memory-size 1000
  "Maximum number of entries in memory cache before LRU eviction."
  :type 'integer
  :group 'ecache)

(defcustom ecache-idle-invalidation-time nil
  "Idle time in seconds before cache is invalidated.
nil means no idle-based invalidation."
  :type '(choice (const :tag "No idle invalidation" nil)
                 (integer :tag "Seconds"))
  :group 'ecache)

(defcustom ecache-auto-save-interval 300
  "Interval in seconds for auto-saving memory cache to disk.
nil means no auto-save."
  :type '(choice (const :tag "No auto-save" nil)
                 (integer :tag "Seconds"))
  :group 'ecache)

;;; Internal Variables

(defvar ecache--global-cache (make-hash-table :test 'equal)
  "Global in-memory cache hash table.")

(defvar ecache--cache-metadata (make-hash-table :test 'equal)
  "Metadata for cache entries (TTL, access time, etc.).")

(defvar ecache--buffer-local-cache-key 'ecache--buffer-cache
  "Symbol for buffer-local cache variable.")

(defvar ecache--idle-timer nil
  "Timer for idle-based cache invalidation.")

(defvar ecache--auto-save-timer nil
  "Timer for auto-saving cache.")

(defvar ecache--access-order nil
  "List tracking access order for LRU eviction.")

;;; Utility Functions

(defun ecache--ensure-directory ()
  "Ensure cache directory exists."
  (unless (file-exists-p ecache-directory)
    (make-directory ecache-directory t)))

(defun ecache--serialize (value)
  "Serialize VALUE to string representation."
  (prin1-to-string value))

(defun ecache--deserialize (string)
  "Deserialize STRING to Lisp value."
  (condition-case nil
      (read string)
    (error nil)))

(defun ecache--current-time ()
  "Return current time in seconds since epoch."
  (float-time))

(defun ecache--expired-p (key)
  "Check if cache entry for KEY has expired."
  (let ((metadata (gethash key ecache--cache-metadata)))
    (when metadata
      (let ((ttl (plist-get metadata :ttl))
            (created (plist-get metadata :created)))
        (and ttl created
             (> (- (ecache--current-time) created) ttl))))))

(defun ecache--update-access (key)
  "Update access time and order for KEY."
  (let ((metadata (gethash key ecache--cache-metadata)))
    (when metadata
      (plist-put metadata :accessed (ecache--current-time))
      (puthash key metadata ecache--cache-metadata))
    ;; Update LRU order
    (setq ecache--access-order
          (cons key (delete key ecache--access-order)))))

(defun ecache--evict-lru ()
  "Evict least recently used entry when cache is full."
  (when (>= (hash-table-count ecache--global-cache) ecache-max-memory-size)
    (let ((lru-key (car (last ecache--access-order))))
      (when lru-key
        (remhash lru-key ecache--global-cache)
        (remhash lru-key ecache--cache-metadata)
        (setq ecache--access-order (delete lru-key ecache--access-order))))))

(defun ecache--get-buffer-local-cache ()
  "Get or create buffer-local cache hash table."
  (unless (local-variable-p ecache--buffer-local-cache-key)
    (set (make-local-variable ecache--buffer-local-cache-key)
         (make-hash-table :test 'equal)))
  (symbol-value ecache--buffer-local-cache-key))

;;; Memory Cache (Global)

(defun ecache-set-memory (key value &optional ttl)
  "Set KEY to VALUE in global memory cache with optional TTL."
  (ecache--evict-lru)
  (puthash key value ecache--global-cache)
  (puthash key (list :created (ecache--current-time)
                     :accessed (ecache--current-time)
                     :ttl ttl)
           ecache--cache-metadata)
  (ecache--update-access key)
  value)

(defun ecache-get-memory (key &optional default)
  "Get value for KEY from global memory cache, return DEFAULT if not found."
  (if (ecache--expired-p key)
      (progn
        (ecache-delete-memory key)
        default)
    (let ((value (gethash key ecache--global-cache default)))
      (when (not (eq value default))
        (ecache--update-access key))
      value)))

(defun ecache-delete-memory (key)
  "Delete KEY from global memory cache."
  (remhash key ecache--global-cache)
  (remhash key ecache--cache-metadata)
  (setq ecache--access-order (delete key ecache--access-order)))

(defun ecache-clear-memory ()
  "Clear all entries in global memory cache."
  (clrhash ecache--global-cache)
  (clrhash ecache--cache-metadata)
  (setq ecache--access-order nil))

(defun ecache-keys-memory ()
  "Return list of all keys in global memory cache."
  (let (keys)
    (maphash (lambda (k _v) (push k keys)) ecache--global-cache)
    keys))

;;; Buffer-Local Cache

(defun ecache-set-buffer-local (key value)
  "Set KEY to VALUE in buffer-local cache."
  (let ((cache (ecache--get-buffer-local-cache)))
    (puthash key value cache)
    value))

(defun ecache-get-buffer-local (key &optional default)
  "Get value for KEY from buffer-local cache, return DEFAULT if not found."
  (let ((cache (ecache--get-buffer-local-cache)))
    (gethash key cache default)))

(defun ecache-delete-buffer-local (key)
  "Delete KEY from buffer-local cache."
  (let ((cache (ecache--get-buffer-local-cache)))
    (remhash key cache)))

(defun ecache-clear-buffer-local ()
  "Clear all entries in buffer-local cache."
  (when (local-variable-p ecache--buffer-local-cache-key)
    (clrhash (symbol-value ecache--buffer-local-cache-key))))

(defun ecache-keys-buffer-local ()
  "Return list of all keys in buffer-local cache."
  (let ((cache (ecache--get-buffer-local-cache))
        keys)
    (maphash (lambda (k _v) (push k keys)) cache)
    keys))

;;; File-Based Cache

(defun ecache--file-cache-path (key)
  "Return file path for cache KEY.
Uses SHA256 hash of key to create a safe filename."
  (unless (stringp key)
    (error "Cache key must be a string"))
  (when (string-empty-p key)
    (error "Cache key cannot be empty"))
  (expand-file-name (concat "ecache-" (secure-hash 'sha256 key)) ecache-directory))

(defun ecache-set-file (key value &optional ttl)
  "Set KEY to VALUE in file-based cache with optional TTL."
  (ecache--ensure-directory)
  (let* ((file (ecache--file-cache-path key))
         (data (list :key key
                     :value value
                     :created (ecache--current-time)
                     :ttl ttl)))
    (with-temp-file file
      (insert (ecache--serialize data)))
    value))

(defun ecache-get-file (key &optional default)
  "Get value for KEY from file-based cache, return DEFAULT if not found."
  (let ((file (ecache--file-cache-path key)))
    (if (file-exists-p file)
        (let* ((data (with-temp-buffer
                       (insert-file-contents file)
                       (ecache--deserialize (buffer-string))))
               (created (plist-get data :created))
               (ttl (plist-get data :ttl))
               (value (plist-get data :value)))
          (if (and ttl created
                   (> (- (ecache--current-time) created) ttl))
              (progn
                (ecache-delete-file key)
                default)
            value))
      default)))

(defun ecache-delete-file (key)
  "Delete KEY from file-based cache."
  (let ((file (ecache--file-cache-path key)))
    (when (file-exists-p file)
      (delete-file file))))

(defun ecache-clear-file ()
  "Clear all entries in file-based cache.
Only deletes files that match the ecache file naming pattern."
  (when (file-exists-p ecache-directory)
    (dolist (file (directory-files ecache-directory t "^ecache-[0-9a-f]\\{64\\}$"))
      (when (file-regular-p file)
        (delete-file file)))))

(defun ecache-keys-file ()
  "Return list of all keys in file-based cache."
  (when (file-exists-p ecache-directory)
    (let (keys)
      (dolist (file (directory-files ecache-directory t "^ecache-[0-9a-f]\\{64\\}$"))
        (when (file-regular-p file)
          (let* ((data (condition-case nil
                           (with-temp-buffer
                             (insert-file-contents file)
                             (ecache--deserialize (buffer-string)))
                         (error nil)))
                 (key (when data (plist-get data :key))))
            (when key
              (push key keys)))))
      keys)))

;;; Database Cache (SQLite)

(defvar ecache--db-available 'unknown
  "Whether SQLite support is available.
Value can be 'unknown, t, or nil.")

(defun ecache--db-available-p ()
  "Check if SQLite support is available."
  (when (eq ecache--db-available 'unknown)
    (setq ecache--db-available
          (and (fboundp 'sqlite-available-p)
               (sqlite-available-p))))
  ecache--db-available)

(defvar ecache--db-connection nil
  "SQLite database connection.")

(defun ecache--db-init ()
  "Initialize database connection and schema."
  (when (ecache--db-available-p)
    (ecache--ensure-directory)
    (unless ecache--db-connection
      (setq ecache--db-connection (sqlite-open ecache-database-file))
      (sqlite-execute ecache--db-connection
                      "CREATE TABLE IF NOT EXISTS cache (
                         key TEXT PRIMARY KEY,
                         value TEXT,
                         created REAL,
                         ttl REAL
                       )"))))

(defun ecache-set-database (key value &optional ttl)
  "Set KEY to VALUE in database cache with optional TTL."
  (when (ecache--db-available-p)
    (ecache--db-init)
    (sqlite-execute ecache--db-connection
                    "INSERT OR REPLACE INTO cache (key, value, created, ttl) VALUES (?, ?, ?, ?)"
                    (list key
                          (ecache--serialize value)
                          (ecache--current-time)
                          ttl))
    value))

(defun ecache-get-database (key &optional default)
  "Get value for KEY from database cache, return DEFAULT if not found."
  (if (not (ecache--db-available-p))
      default
    (ecache--db-init)
    (let ((result (sqlite-select ecache--db-connection
                                 "SELECT value, created, ttl FROM cache WHERE key = ?"
                                 (list key))))
      (if result
          (let* ((row (car result))
                 (value-str (nth 0 row))
                 (created (nth 1 row))
                 (ttl (nth 2 row))
                 (value (ecache--deserialize value-str)))
            (if (and ttl created
                     (> (- (ecache--current-time) created) ttl))
                (progn
                  (ecache-delete-database key)
                  default)
              value))
        default))))

(defun ecache-delete-database (key)
  "Delete KEY from database cache."
  (when (ecache--db-available-p)
    (ecache--db-init)
    (sqlite-execute ecache--db-connection
                    "DELETE FROM cache WHERE key = ?"
                    (list key))))

(defun ecache-clear-database ()
  "Clear all entries in database cache."
  (when (ecache--db-available-p)
    (ecache--db-init)
    (sqlite-execute ecache--db-connection "DELETE FROM cache")))

(defun ecache-keys-database ()
  "Return list of all keys in database cache."
  (if (not (ecache--db-available-p))
      nil
    (ecache--db-init)
    (let ((result (sqlite-select ecache--db-connection
                                 "SELECT key FROM cache")))
      (mapcar #'car result))))

;;; Unified API

(defun ecache-set (key value &rest args)
  "Set KEY to VALUE in cache.

ARGS is a plist with the following optional keys:
  :backend   - Cache backend: 'memory (default), 'file, or 'database
  :ttl       - Time to live in seconds
  :buffer-local - If non-nil, use buffer-local cache (overrides :backend)

Examples:
  (ecache-set \"key\" \"value\")
  (ecache-set \"key\" \"value\" :ttl 60)
  (ecache-set \"key\" \"value\" :backend 'file)
  (ecache-set \"key\" \"value\" :buffer-local t)"
  (let ((backend (or (plist-get args :backend) 'memory))
        (ttl (or (plist-get args :ttl) ecache-default-ttl))
        (buffer-local (plist-get args :buffer-local)))
    (cond
     (buffer-local
      (ecache-set-buffer-local key value))
     ((eq backend 'file)
      (ecache-set-file key value ttl))
     ((eq backend 'database)
      (ecache-set-database key value ttl))
     (t
      (ecache-set-memory key value ttl)))))

(defun ecache-get (key &rest args)
  "Get value for KEY from cache.

ARGS is a plist with the following optional keys:
  :backend   - Cache backend: 'memory (default), 'file, or 'database
  :default   - Default value if key not found
  :buffer-local - If non-nil, use buffer-local cache (overrides :backend)

Examples:
  (ecache-get \"key\")
  (ecache-get \"key\" :default \"default-value\")
  (ecache-get \"key\" :backend 'file)
  (ecache-get \"key\" :buffer-local t)"
  (let ((backend (or (plist-get args :backend) 'memory))
        (default (plist-get args :default))
        (buffer-local (plist-get args :buffer-local)))
    (cond
     (buffer-local
      (ecache-get-buffer-local key default))
     ((eq backend 'file)
      (ecache-get-file key default))
     ((eq backend 'database)
      (ecache-get-database key default))
     (t
      (ecache-get-memory key default)))))

(defun ecache-delete (key &rest args)
  "Delete KEY from cache.

ARGS is a plist with the following optional keys:
  :backend   - Cache backend: 'memory (default), 'file, or 'database
  :buffer-local - If non-nil, use buffer-local cache (overrides :backend)

Examples:
  (ecache-delete \"key\")
  (ecache-delete \"key\" :backend 'file)
  (ecache-delete \"key\" :buffer-local t)"
  (let ((backend (or (plist-get args :backend) 'memory))
        (buffer-local (plist-get args :buffer-local)))
    (cond
     (buffer-local
      (ecache-delete-buffer-local key))
     ((eq backend 'file)
      (ecache-delete-file key))
     ((eq backend 'database)
      (ecache-delete-database key))
     (t
      (ecache-delete-memory key)))))

(defun ecache-clear (&rest args)
  "Clear all entries from cache.

ARGS is a plist with the following optional keys:
  :backend   - Cache backend: 'memory (default), 'file, 'database, or 'all
  :buffer-local - If non-nil, use buffer-local cache (overrides :backend)

Examples:
  (ecache-clear)
  (ecache-clear :backend 'file)
  (ecache-clear :backend 'all)
  (ecache-clear :buffer-local t)"
  (let ((backend (or (plist-get args :backend) 'memory))
        (buffer-local (plist-get args :buffer-local)))
    (cond
     (buffer-local
      (ecache-clear-buffer-local))
     ((eq backend 'all)
      (ecache-clear-memory)
      (ecache-clear-file)
      (ecache-clear-database))
     ((eq backend 'file)
      (ecache-clear-file))
     ((eq backend 'database)
      (ecache-clear-database))
     (t
      (ecache-clear-memory)))))

(defun ecache-keys (&rest args)
  "Return list of all keys in cache.

ARGS is a plist with the following optional keys:
  :backend   - Cache backend: 'memory (default), 'file, or 'database
  :buffer-local - If non-nil, use buffer-local cache (overrides :backend)

Examples:
  (ecache-keys)
  (ecache-keys :backend 'file)
  (ecache-keys :buffer-local t)"
  (let ((backend (or (plist-get args :backend) 'memory))
        (buffer-local (plist-get args :buffer-local)))
    (cond
     (buffer-local
      (ecache-keys-buffer-local))
     ((eq backend 'file)
      (ecache-keys-file))
     ((eq backend 'database)
      (ecache-keys-database))
     (t
      (ecache-keys-memory)))))

;;; Cache Invalidation Strategies

(defun ecache-invalidate-on-hook (hook backend &optional keys)
  "Invalidate cache entries on HOOK for BACKEND.
If KEYS is provided, only invalidate those keys, otherwise clear all."
  (add-hook hook
            (lambda ()
              (if keys
                  (dolist (key keys)
                    (ecache-delete key :backend backend))
                (ecache-clear :backend backend)))))

(defun ecache-invalidate-on-idle ()
  "Set up idle-based cache invalidation."
  (when ecache-idle-invalidation-time
    (when ecache--idle-timer
      (cancel-timer ecache--idle-timer))
    (setq ecache--idle-timer
          (run-with-idle-timer ecache-idle-invalidation-time t
                               #'ecache-clear-memory))))

(defun ecache-setup-auto-save ()
  "Set up automatic saving of memory cache to file."
  (when ecache-auto-save-interval
    (when ecache--auto-save-timer
      (cancel-timer ecache--auto-save-timer))
    (setq ecache--auto-save-timer
          (run-with-timer ecache-auto-save-interval
                          ecache-auto-save-interval
                          #'ecache-save-to-file))))

(defun ecache-save-to-file (&optional file)
  "Save memory cache to FILE (defaults to ecache-directory/memory-cache.el)."
  (ecache--ensure-directory)
  (let ((file (or file (expand-file-name "memory-cache.el" ecache-directory)))
        (data (list :cache ecache--global-cache
                    :metadata ecache--cache-metadata
                    :access-order ecache--access-order)))
    (with-temp-file file
      (insert (ecache--serialize data)))))

(defun ecache-load-from-file (&optional file)
  "Load memory cache from FILE (defaults to ecache-directory/memory-cache.el)."
  (let ((file (or file (expand-file-name "memory-cache.el" ecache-directory))))
    (when (file-exists-p file)
      (let ((data (with-temp-buffer
                    (insert-file-contents file)
                    (ecache--deserialize (buffer-string)))))
        (setq ecache--global-cache (or (plist-get data :cache)
                                       (make-hash-table :test 'equal))
              ecache--cache-metadata (or (plist-get data :metadata)
                                         (make-hash-table :test 'equal))
              ecache--access-order (or (plist-get data :access-order) nil))))))

;;; Interactive Functions

;;;###autoload
(defun ecache-inspect-memory ()
  "Interactively inspect memory cache contents."
  (interactive)
  (let ((keys (ecache-keys-memory)))
    (if (null keys)
        (message "Memory cache is empty")
      (with-current-buffer (get-buffer-create "*ecache-memory*")
        (erase-buffer)
        (insert "Memory Cache Contents\n")
        (insert "=====================\n\n")
        (dolist (key keys)
          (let* ((value (ecache-get-memory key))
                 (metadata (gethash key ecache--cache-metadata))
                 (created (plist-get metadata :created))
                 (ttl (plist-get metadata :ttl)))
            (insert (format "Key: %s\n" key))
            (insert (format "Value: %S\n" value))
            (insert (format "Created: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S"
                                                                 (seconds-to-time created))))
            (when ttl
              (insert (format "TTL: %d seconds\n" ttl)))
            (insert "\n")))
        (goto-char (point-min))
        (display-buffer (current-buffer))))))

;;;###autoload
(defun ecache-inspect-buffer-local ()
  "Interactively inspect buffer-local cache contents."
  (interactive)
  (let ((keys (ecache-keys-buffer-local)))
    (if (null keys)
        (message "Buffer-local cache is empty")
      (with-current-buffer (get-buffer-create "*ecache-buffer-local*")
        (erase-buffer)
        (insert "Buffer-Local Cache Contents\n")
        (insert "===========================\n\n")
        (dolist (key keys)
          (let ((value (ecache-get-buffer-local key)))
            (insert (format "Key: %s\n" key))
            (insert (format "Value: %S\n\n" value))))
        (goto-char (point-min))
        (display-buffer (current-buffer))))))

;;;###autoload
(defun ecache-stats ()
  "Display cache statistics."
  (interactive)
  (let ((memory-count (hash-table-count ecache--global-cache))
        (file-count (length (ecache-keys-file)))
        (db-count (if (ecache--db-available-p)
                      (length (ecache-keys-database))
                    0)))
    (message "Cache Statistics: Memory=%d File=%d Database=%d"
             memory-count file-count db-count)))

;;; Initialization

;;;###autoload
(defun ecache-init ()
  "Initialize ecache system."
  (interactive)
  (ecache-invalidate-on-idle)
  (ecache-setup-auto-save)
  (ecache-load-from-file))

(provide 'ecache)

;;; ecache.el ends here
