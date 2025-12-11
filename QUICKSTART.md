# ecache Quick Start Guide

Get up and running with ecache in 5 minutes!

## Installation

### Step 1: Download

Clone the repository or download `ecache.el`:

```bash
git clone https://github.com/Kinneyzhang/ecache.git
```

### Step 2: Add to Emacs Config

Add to your `init.el` or `.emacs`:

```elisp
(add-to-list 'load-path "/path/to/ecache")
(require 'ecache)
(ecache-init)  ; Initialize the cache system
```

### Step 3: Restart Emacs

Restart Emacs or evaluate the config: `M-x eval-buffer`

## Basic Usage

### Store and Retrieve Data

```elisp
;; Store data
(ecache-set "username" "john_doe")
(ecache-set "user-age" 30)
(ecache-set "user-preferences" '((theme . "dark") (font . "Monaco")))

;; Retrieve data
(ecache-get "username")         ; => "john_doe"
(ecache-get "user-age")         ; => 30
(ecache-get "user-preferences") ; => ((theme . "dark") (font . "Monaco"))
```

### Use Default Values

```elisp
;; Get with default value if key doesn't exist
(ecache-get "nonexistent-key" :default "default-value")
; => "default-value"
```

### Set Expiration Time (TTL)

```elisp
;; Cache expires in 60 seconds
(ecache-set "temp-data" "expires soon" :ttl 60)

;; After 60 seconds, returns nil
(sleep-for 61)
(ecache-get "temp-data")  ; => nil
```

## Storage Backends

### Memory Cache (Default - Fast)

```elisp
;; Fast, but lost on Emacs restart
(ecache-set "session-id" "abc123")
(ecache-get "session-id")
```

### File Cache (Persistent)

```elisp
;; Survives Emacs restart
(ecache-set "api-token" "secret-token-here" :backend 'file)
(ecache-get "api-token" :backend 'file)
```

### Database Cache (Requires Emacs 29+)

```elisp
;; Good for large datasets
(ecache-set "large-data" my-big-list :backend 'database)
(ecache-get "large-data" :backend 'database)
```

### Buffer-Local Cache

```elisp
;; Specific to current buffer
(ecache-set "buffer-state" "editing" :buffer-local t)
(ecache-get "buffer-state" :buffer-local t)
```

## Common Patterns

### Caching API Results

```elisp
(defun fetch-user-data (user-id)
  "Fetch user data with 1-hour cache."
  (let ((cache-key (format "user:%s" user-id)))
    (or (ecache-get cache-key :backend 'file)
        (let ((data (api-call-to-fetch-user user-id)))
          (ecache-set cache-key data :backend 'file :ttl 3600)
          data))))
```

### Memoization

```elisp
(defun expensive-calculation (input)
  "Calculate with caching."
  (let ((cache-key (format "calc:%S" input)))
    (or (ecache-get cache-key)
        (let ((result (really-expensive-operation input)))
          (ecache-set cache-key result :ttl 600)
          result))))
```

### Session State

```elisp
;; Save state before closing
(defun save-my-session ()
  (ecache-set "last-file" (buffer-file-name) :backend 'file)
  (ecache-set "last-position" (point) :backend 'file))

;; Restore on startup
(defun restore-my-session ()
  (let ((file (ecache-get "last-file" :backend 'file))
        (pos (ecache-get "last-position" :backend 'file)))
    (when (and file (file-exists-p file))
      (find-file file)
      (goto-char pos))))
```

## Management

### View Cache Contents

```elisp
;; Inspect memory cache
M-x ecache-inspect-memory

;; Inspect buffer-local cache
M-x ecache-inspect-buffer-local

;; Show statistics
M-x ecache-stats
```

### Clear Cache

```elisp
;; Clear memory cache
(ecache-clear)

;; Clear file cache
(ecache-clear :backend 'file)

;; Clear everything
(ecache-clear :backend 'all)
```

### Delete Specific Keys

```elisp
;; Delete from memory
(ecache-delete "my-key")

;; Delete from file
(ecache-delete "my-key" :backend 'file)
```

### List All Keys

```elisp
;; List memory cache keys
(ecache-keys)  ; => ("key1" "key2" "key3")

;; List file cache keys
(ecache-keys :backend 'file)
```

## Configuration

Add to your config for customization:

```elisp
;; Change cache directory
(setq ecache-directory "~/.cache/emacs-ecache")

;; Set default TTL to 1 hour
(setq ecache-default-ttl 3600)

;; Set max memory cache size
(setq ecache-max-memory-size 1000)

;; Auto-clear cache after 10 minutes idle
(setq ecache-idle-invalidation-time 600)

;; Auto-save every 5 minutes
(setq ecache-auto-save-interval 300)

;; Re-initialize after changing settings
(ecache-init)
```

## Cache Invalidation

### On Hook

```elisp
;; Clear cache when saving files
(ecache-invalidate-on-hook 'after-save-hook 'memory)

;; Clear specific keys on hook
(ecache-invalidate-on-hook 'after-save-hook 'memory '("key1" "key2"))
```

### Manual Save/Load

```elisp
;; Save memory cache to file
(ecache-save-to-file)

;; Load memory cache from file
(ecache-load-from-file)
```

## Tips

1. **Use memory cache** for frequently accessed data
2. **Use file cache** for data that needs to persist
3. **Use buffer-local cache** for buffer-specific state
4. **Set appropriate TTLs** to keep data fresh
5. **Clear caches periodically** to prevent stale data

## Next Steps

- Read [README.md](README.md) for detailed documentation
- Check [examples.el](examples.el) for more patterns
- See [ARCHITECTURE.md](ARCHITECTURE.md) for internals

## Need Help?

- Check the [documentation](README.md)
- Look at [examples](examples.el)
- Open an [issue](https://github.com/Kinneyzhang/ecache/issues)

## Quick Reference

```elisp
;; Core Operations
(ecache-set KEY VALUE &rest ARGS)
(ecache-get KEY &rest ARGS)
(ecache-delete KEY &rest ARGS)
(ecache-clear &rest ARGS)
(ecache-keys &rest ARGS)

;; Common Arguments
:backend 'memory|'file|'database
:ttl SECONDS
:default VALUE
:buffer-local t

;; Examples
(ecache-set "key" "value")
(ecache-set "key" "value" :ttl 60)
(ecache-set "key" "value" :backend 'file)
(ecache-get "key" :default "fallback")
(ecache-delete "key")
(ecache-clear)
```

Happy caching! 🚀
