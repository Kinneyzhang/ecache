# ecache - Universal Cache System for Emacs

[中文文档](README-CN.md)

A powerful and easy-to-use cache system for Emacs, inspired by browser localStorage/sessionStorage and excellent caching libraries from other languages.

## Features

- **Multiple Storage Backends**
  - Memory cache (global and buffer-local)
  - File-based cache (persistent)
  - Database cache (SQLite, requires Emacs 29+)

- **Flexible Cache Strategies**
  - TTL (Time To Live) expiration
  - LRU (Least Recently Used) eviction policy
  - Idle-time based invalidation
  - Hook-triggered cache invalidation

- **Simple API**
  - localStorage-like key-value interface
  - Automatic serialization/deserialization
  - Support for any Lisp data types

- **Automatic Persistence**
  - Periodic auto-save of memory cache to disk
  - Auto-load on startup

## Installation

### Manual Installation

Download `ecache.el` to your Emacs configuration directory and add to your `init.el`:

```elisp
(add-to-list 'load-path "/path/to/ecache")
(require 'ecache)
(ecache-init)  ; Initialize cache system
```

### Configuration

```elisp
;; Customize cache directory
(setq ecache-directory (expand-file-name "cache" user-emacs-directory))

;; Set default TTL (in seconds)
(setq ecache-default-ttl 3600)  ; 1 hour

;; Set maximum memory cache size
(setq ecache-max-memory-size 1000)

;; Set idle invalidation time (in seconds)
(setq ecache-idle-invalidation-time 600)  ; 10 minutes

;; Set auto-save interval (in seconds)
(setq ecache-auto-save-interval 300)  ; 5 minutes
```

## Usage

### Basic Operations

#### Memory Cache (Global)

```elisp
;; Set cache
(ecache-set "my-key" "my-value")

;; Get cache
(ecache-get "my-key")  ; => "my-value"

;; Get cache with default value
(ecache-get "nonexistent-key" :default "default-value")  ; => "default-value"

;; Delete cache
(ecache-delete "my-key")

;; Clear all cache
(ecache-clear)

;; Get all keys
(ecache-keys)  ; => ("key1" "key2" ...)
```

#### Cache with TTL

```elisp
;; Set cache that expires in 60 seconds
(ecache-set "temp-key" "temp-value" :ttl 60)

;; Valid within 60 seconds
(ecache-get "temp-key")  ; => "temp-value"

;; Automatically expired after 60 seconds
(sleep-for 61)
(ecache-get "temp-key")  ; => nil
```

#### Buffer-Local Cache

```elisp
;; Set cache in current buffer
(ecache-set "buffer-key" "buffer-value" :buffer-local t)

;; Get cache from current buffer
(ecache-get "buffer-key" :buffer-local t)  ; => "buffer-value"

;; Cache is not shared in other buffers
(with-current-buffer other-buffer
  (ecache-get "buffer-key" :buffer-local t))  ; => nil
```

### File Cache

File cache persists data to disk, surviving Emacs restarts.

```elisp
;; Set file cache
(ecache-set "persistent-key" "persistent-value" :backend 'file)

;; Get file cache
(ecache-get "persistent-key" :backend 'file)  ; => "persistent-value"

;; Delete file cache
(ecache-delete "persistent-key" :backend 'file)

;; Clear all file cache
(ecache-clear :backend 'file)

;; Get all file cache keys
(ecache-keys :backend 'file)
```

### Database Cache (Requires Emacs 29+)

Database cache uses SQLite, suitable for large datasets.

```elisp
;; Set database cache
(ecache-set "db-key" "db-value" :backend 'database)

;; Get database cache
(ecache-get "db-key" :backend 'database)  ; => "db-value"

;; Delete database cache
(ecache-delete "db-key" :backend 'database)

;; Clear all database cache
(ecache-clear :backend 'database)

;; Get all database cache keys
(ecache-keys :backend 'database)
```

### Supported Data Types

ecache supports any serializable Lisp data types:

```elisp
;; String
(ecache-set "string" "Hello, World!")

;; Number
(ecache-set "number" 42)

;; List
(ecache-set "list" '(1 2 3 4 5))

;; Association list
(ecache-set "alist" '((name . "John") (age . 30)))

;; Property list
(ecache-set "plist" '(:name "John" :age 30))

;; Hash table
(let ((hash (make-hash-table :test 'equal)))
  (puthash "key" "value" hash)
  (ecache-set "hash" hash))

;; Vector
(ecache-set "vector" [1 2 3 4 5])
```

## Advanced Usage

### Cache Invalidation Strategies

#### Hook-Triggered Invalidation

```elisp
;; Clear cache when hook is triggered
(ecache-invalidate-on-hook 'after-save-hook 'memory)

;; Delete only specific keys when hook is triggered
(ecache-invalidate-on-hook 'after-save-hook 'memory '("key1" "key2"))

;; Clear file cache after saving
(ecache-invalidate-on-hook 'after-save-hook 'file)
```

#### Idle-Time Invalidation

```elisp
;; Auto-clear memory cache after 10 minutes of idle time
(setq ecache-idle-invalidation-time 600)
(ecache-invalidate-on-idle)
```

#### Auto-Save

```elisp
;; Auto-save memory cache to file every 5 minutes
(setq ecache-auto-save-interval 300)
(ecache-setup-auto-save)
```

### Manual Save and Load

```elisp
;; Save memory cache to file
(ecache-save-to-file)

;; Load memory cache from file
(ecache-load-from-file)

;; Use custom file path
(ecache-save-to-file "/path/to/cache-backup.el")
(ecache-load-from-file "/path/to/cache-backup.el")
```

### Inspection and Debugging

```elisp
;; Inspect memory cache contents
(ecache-inspect-memory)

;; Inspect buffer-local cache contents
(ecache-inspect-buffer-local)

;; Display cache statistics
(ecache-stats)  ; => "Cache Statistics: Memory=10 File=5 Database=3"
```

## Real-World Examples

### Caching API Request Results

```elisp
(defun my-fetch-data (url)
  "Fetch data from URL with caching to avoid duplicate requests."
  (let ((cache-key (format "api:%s" url)))
    (or (ecache-get cache-key :backend 'file)
        (let ((data (my-api-request url)))
          (ecache-set cache-key data :backend 'file :ttl 3600)
          data))))
```

### Caching Expensive Computations

```elisp
(defun my-expensive-computation (input)
  "Perform expensive computation with result caching."
  (let ((cache-key (format "compute:%S" input)))
    (or (ecache-get cache-key)
        (let ((result (my-compute input)))
          (ecache-set cache-key result :ttl 600)
          result))))
```

### Buffer-Local Configuration

```elisp
(defun my-buffer-config-set (key value)
  "Set buffer-local configuration."
  (ecache-set key value :buffer-local t))

(defun my-buffer-config-get (key &optional default)
  "Get buffer-local configuration."
  (ecache-get key :buffer-local t :default default))
```

### Project-Level Cache

```elisp
(defun my-project-cache-set (key value)
  "Set project-level cache."
  (let ((project-root (project-root (project-current))))
    (ecache-set (format "%s:%s" project-root key) 
                value 
                :backend 'file)))

(defun my-project-cache-get (key)
  "Get project-level cache."
  (let ((project-root (project-root (project-current))))
    (ecache-get (format "%s:%s" project-root key)
                :backend 'file)))
```

## Performance Considerations

### Choosing the Right Backend

- **Memory cache**: Fastest, suitable for frequently accessed small data
- **File cache**: Medium speed, suitable for persistent medium-sized data
- **Database cache**: Suitable for large datasets, supports complex queries (requires Emacs 29+)

### LRU Eviction Policy

When memory cache reaches `ecache-max-memory-size`, it automatically evicts the least recently used entries.

```elisp
;; Set maximum cache entries
(setq ecache-max-memory-size 1000)
```

## API Reference

### Core Functions

- `(ecache-set KEY VALUE &rest ARGS)` - Set cache
- `(ecache-get KEY &rest ARGS)` - Get cache
- `(ecache-delete KEY &rest ARGS)` - Delete cache
- `(ecache-clear &rest ARGS)` - Clear cache
- `(ecache-keys &rest ARGS)` - Get all keys

### Arguments

ARGS is a plist supporting the following keys:

- `:backend` - Backend type: `'memory` (default), `'file`, or `'database`
- `:ttl` - Time to live in seconds
- `:default` - Default value (only for `ecache-get`)
- `:buffer-local` - Whether to use buffer-local cache (overrides `:backend`)

### Backend-Specific Functions

Memory cache:
- `ecache-set-memory`, `ecache-get-memory`, `ecache-delete-memory`
- `ecache-clear-memory`, `ecache-keys-memory`

Buffer-local cache:
- `ecache-set-buffer-local`, `ecache-get-buffer-local`, `ecache-delete-buffer-local`
- `ecache-clear-buffer-local`, `ecache-keys-buffer-local`

File cache:
- `ecache-set-file`, `ecache-get-file`, `ecache-delete-file`
- `ecache-clear-file`, `ecache-keys-file`

Database cache:
- `ecache-set-database`, `ecache-get-database`, `ecache-delete-database`
- `ecache-clear-database`, `ecache-keys-database`

### Management Functions

- `(ecache-init)` - Initialize cache system
- `(ecache-save-to-file &optional FILE)` - Save memory cache to file
- `(ecache-load-from-file &optional FILE)` - Load memory cache from file
- `(ecache-invalidate-on-hook HOOK BACKEND &optional KEYS)` - Setup hook-based invalidation
- `(ecache-invalidate-on-idle)` - Setup idle-based invalidation
- `(ecache-setup-auto-save)` - Setup auto-save

### Interactive Commands

- `M-x ecache-inspect-memory` - Inspect memory cache contents
- `M-x ecache-inspect-buffer-local` - Inspect buffer-local cache contents
- `M-x ecache-stats` - Display cache statistics
- `M-x ecache-init` - Initialize cache system

## Design Philosophy

ecache's design is inspired by:

1. **Browser Storage API**: Simple key-value interface like localStorage/sessionStorage
2. **Redis**: TTL support and multiple backends
3. **Python functools.lru_cache**: LRU eviction policy
4. **Memcached**: Layered caching strategy

## FAQ

### Database cache not available?

Database cache requires Emacs 29+ with built-in SQLite support. Check:

```elisp
(sqlite-available-p)  ; Should return t
```

### How to clear all caches?

```elisp
;; Clear all backend caches
(ecache-clear :backend 'all)
```

### Where are cache files stored?

Default is `~/.emacs.d/ecache/` directory. Customize via `ecache-directory`.

## License

GPL-3.0 or later

## Contributing

Issues and pull requests are welcome!