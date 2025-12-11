;;; examples.el --- Usage examples for ecache -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains practical examples of using ecache in real-world scenarios.

;;; Code:

(require 'ecache)

;;; Example 1: Caching API Responses

(defun example-fetch-user-data (user-id)
  "Fetch user data with caching to reduce API calls.
Caches result for 1 hour."
  (let ((cache-key (format "user:%s" user-id)))
    (or (ecache-get cache-key :backend 'file)
        (progn
          (message "Fetching user %s from API..." user-id)
          (let ((data (format "User data for %s" user-id)))  ; Simulated API call
            (ecache-set cache-key data :backend 'file :ttl 3600)
            data)))))

;; Usage:
;; (example-fetch-user-data "12345")  ; Fetches from API
;; (example-fetch-user-data "12345")  ; Returns cached result

;;; Example 2: Memoization for Expensive Computations

(defun example-fibonacci (n)
  "Calculate Fibonacci number with memoization."
  (let ((cache-key (format "fib:%d" n)))
    (or (ecache-get cache-key)
        (let ((result (if (<= n 1)
                          n
                        (+ (example-fibonacci (- n 1))
                           (example-fibonacci (- n 2))))))
          (ecache-set cache-key result)
          result))))

;; Usage:
;; (example-fibonacci 30)  ; First call is slow
;; (example-fibonacci 30)  ; Subsequent calls are instant

;;; Example 3: Buffer-Local State Management

(defun example-buffer-note-set (note)
  "Set a note for the current buffer."
  (interactive "sNote: ")
  (ecache-set "buffer-note" note :buffer-local t)
  (message "Note saved for this buffer"))

(defun example-buffer-note-get ()
  "Get the note for the current buffer."
  (interactive)
  (let ((note (ecache-get "buffer-note" :buffer-local t)))
    (if note
        (message "Buffer note: %s" note)
      (message "No note for this buffer"))))

;; Usage:
;; M-x example-buffer-note-set RET "This is my note" RET
;; M-x example-buffer-note-get

;;; Example 4: Session Data Persistence

(defun example-save-session-data ()
  "Save session-specific data across Emacs restarts."
  (ecache-set "last-used-file" (buffer-file-name) :backend 'file)
  (ecache-set "last-position" (point) :backend 'file)
  (ecache-set "last-time" (current-time-string) :backend 'file))

(defun example-restore-session-data ()
  "Restore session data from previous session."
  (interactive)
  (let ((file (ecache-get "last-used-file" :backend 'file))
        (pos (ecache-get "last-position" :backend 'file))
        (time (ecache-get "last-time" :backend 'file)))
    (when file
      (message "Last session: file=%s position=%s time=%s" file pos time)
      (when (file-exists-p file)
        (find-file file)
        (when pos (goto-char pos))))))

;; Usage:
;; (example-save-session-data)      ; Save before closing Emacs
;; (example-restore-session-data)   ; Restore after starting Emacs

;;; Example 5: Project-Specific Cache

(defun example-project-tags-get ()
  "Get cached tags for current project."
  (let* ((project (project-current))
         (project-root (when project (project-root project)))
         (cache-key (format "tags:%s" project-root)))
    (when project-root
      (or (ecache-get cache-key :backend 'file)
          (progn
            (message "Building tags for project %s..." project-root)
            (let ((tags (list "tag1" "tag2" "tag3")))  ; Simulated tag generation
              (ecache-set cache-key tags :backend 'file :ttl 7200)
              tags))))))

(defun example-project-tags-invalidate ()
  "Invalidate cached tags for current project."
  (interactive)
  (let* ((project (project-current))
         (project-root (when project (project-root project)))
         (cache-key (format "tags:%s" project-root)))
    (when project-root
      (ecache-delete cache-key :backend 'file)
      (message "Tags cache invalidated for %s" project-root))))

;; Usage:
;; (example-project-tags-get)          ; First call builds tags
;; (example-project-tags-get)          ; Returns cached tags
;; (example-project-tags-invalidate)   ; Clear cache to rebuild

;;; Example 6: Rate Limiting

(defvar example-rate-limit-window 60
  "Rate limit window in seconds.")

(defvar example-rate-limit-max-calls 10
  "Maximum number of calls allowed in the rate limit window.")

(defun example-rate-limited-function ()
  "A rate-limited function that can only be called N times per minute."
  (interactive)
  (let* ((cache-key "rate-limit:calls")
         (calls (or (ecache-get cache-key) '())))
    ;; Remove old timestamps outside the window
    (let ((current-time (float-time))
          (cutoff-time (- (float-time) example-rate-limit-window)))
      (setq calls (cl-remove-if (lambda (time) (< time cutoff-time)) calls))
      
      (if (>= (length calls) example-rate-limit-max-calls)
          (message "Rate limit exceeded! Try again later.")
        ;; Record this call
        (push current-time calls)
        (ecache-set cache-key calls :ttl example-rate-limit-window)
        (message "Function executed. Remaining calls: %d"
                 (- example-rate-limit-max-calls (length calls)))))))

;; Usage:
;; (example-rate-limited-function)  ; Can call 10 times per minute

;;; Example 7: Intelligent Cache Invalidation

(defun example-setup-cache-invalidation ()
  "Set up intelligent cache invalidation based on various triggers."
  ;; Clear temporary caches on idle
  (setq ecache-idle-invalidation-time 600)  ; 10 minutes
  (ecache-invalidate-on-idle)
  
  ;; Invalidate specific caches when files are saved
  (ecache-invalidate-on-hook 'after-save-hook 'memory '("temp-data"))
  
  ;; Auto-save important caches periodically
  (setq ecache-auto-save-interval 300)  ; 5 minutes
  (ecache-setup-auto-save))

;; Usage:
;; (example-setup-cache-invalidation)

;;; Example 8: Multi-Level Cache

(defun example-multi-level-get (key)
  "Get value from multi-level cache (memory -> file -> compute)."
  (or
   ;; Level 1: Try memory cache (fastest)
   (ecache-get key :backend 'memory)
   
   ;; Level 2: Try file cache (persistent)
   (let ((value (ecache-get key :backend 'file)))
     (when value
       ;; Promote to memory cache for faster access
       (ecache-set key value :backend 'memory :ttl 300))
     value)
   
   ;; Level 3: Compute and cache at all levels
   (let ((value (format "Computed value for %s" key)))
     (ecache-set key value :backend 'file :ttl 3600)
     (ecache-set key value :backend 'memory :ttl 300)
     value)))

;; Usage:
;; (example-multi-level-get "my-key")  ; First call computes
;; (example-multi-level-get "my-key")  ; Second call uses memory cache

;;; Example 9: Cache Statistics and Monitoring

(defun example-cache-report ()
  "Generate a comprehensive cache usage report."
  (interactive)
  (let ((memory-keys (ecache-keys :backend 'memory))
        (file-keys (ecache-keys :backend 'file))
        (db-keys (when (ecache--db-available-p)
                   (ecache-keys :backend 'database))))
    (with-current-buffer (get-buffer-create "*ecache-report*")
      (erase-buffer)
      (insert "=== ecache Usage Report ===\n\n")
      (insert (format "Memory cache: %d entries\n" (length memory-keys)))
      (insert (format "File cache: %d entries\n" (length file-keys)))
      (when db-keys
        (insert (format "Database cache: %d entries\n" (length db-keys))))
      (insert "\n--- Memory Cache Keys ---\n")
      (dolist (key memory-keys)
        (insert (format "  - %s\n" key)))
      (insert "\n--- File Cache Keys ---\n")
      (dolist (key file-keys)
        (insert (format "  - %s\n" key)))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;; Usage:
;; M-x example-cache-report

;;; Example 10: Configuration Management

(defvar example-config-backend 'file
  "Backend to use for configuration storage.")

(defun example-config-set (key value)
  "Set configuration KEY to VALUE."
  (interactive "sConfig key: \nsConfig value: ")
  (ecache-set (format "config:%s" key) value
              :backend example-config-backend)
  (message "Configuration %s set to %s" key value))

(defun example-config-get (key &optional default)
  "Get configuration KEY, return DEFAULT if not found."
  (ecache-get (format "config:%s" key)
              :backend example-config-backend
              :default default))

(defun example-config-list ()
  "List all configuration keys."
  (interactive)
  (let* ((all-keys (ecache-keys :backend example-config-backend))
         (config-keys (cl-remove-if-not
                       (lambda (k) (string-prefix-p "config:" k))
                       all-keys)))
    (if config-keys
        (message "Configurations: %s"
                 (mapconcat (lambda (k) (substring k 7)) config-keys ", "))
      (message "No configurations found"))))

;; Usage:
;; (example-config-set "theme" "doom-one")
;; (example-config-get "theme")  ; => "doom-one"
;; M-x example-config-list

(provide 'examples)

;;; examples.el ends here
