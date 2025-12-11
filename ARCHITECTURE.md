# ecache Architecture Documentation

## Overview

ecache is designed as a universal caching system for Emacs, providing multiple storage backends and flexible cache management strategies. This document explains the internal architecture and design decisions.

## Core Components

### 1. Storage Backends

#### Memory Cache (Global)
- **Implementation**: Hash table (`ecache--global-cache`)
- **Metadata**: Separate hash table for TTL, access time (`ecache--cache-metadata`)
- **Access Order**: List tracking LRU order (`ecache--access-order`)
- **Advantages**: Fastest access, no I/O overhead
- **Disadvantages**: Lost on Emacs restart, limited by memory

#### Buffer-Local Cache
- **Implementation**: Buffer-local hash table variable
- **Scope**: Isolated per buffer
- **Use Case**: Buffer-specific temporary data
- **Advantages**: Automatic cleanup when buffer is killed
- **Disadvantages**: Not persistent, separate from global cache

#### File-Based Cache
- **Implementation**: Individual files in `ecache-directory`
- **Storage Format**: Serialized Lisp data (using `prin1-to-string`)
- **File Naming**: SHA256 hash of the key
- **Advantages**: Persistent across restarts, simple implementation
- **Disadvantages**: One file per entry (potential filesystem limits)

#### Database Cache (SQLite)
- **Implementation**: SQLite database via Emacs 29+ built-in support
- **Schema**: Single table with columns: key, value, created, ttl
- **Advantages**: Efficient for large datasets, ACID properties
- **Disadvantages**: Requires Emacs 29+, slightly slower than memory

### 2. Data Structures

```elisp
;; Global cache: key -> value
ecache--global-cache: hash-table

;; Metadata: key -> plist (:created time :accessed time :ttl seconds)
ecache--cache-metadata: hash-table

;; LRU tracking: list of keys in access order (most recent first)
ecache--access-order: list

;; Buffer-local cache: per-buffer hash table
ecache--buffer-cache: buffer-local variable
```

### 3. Serialization

ecache uses Emacs Lisp's built-in serialization:

- **Serialization**: `prin1-to-string` - converts any Lisp object to string
- **Deserialization**: `read` - parses string back to Lisp object
- **Supported Types**: All Lisp data types (strings, numbers, lists, hash tables, vectors, etc.)
- **Limitations**: Cannot serialize buffers, processes, or other non-data objects

### 4. Cache Invalidation

#### TTL (Time To Live)
```elisp
;; Check on access if entry has expired
(when (and ttl created (> (- current-time created) ttl))
  (delete-entry))
```

#### LRU Eviction
```elisp
;; When cache is full (>= ecache-max-memory-size)
;; 1. Find least recently used key (last in access order list)
;; 2. Delete that entry
;; 3. Add new entry
```

#### Idle-Based Invalidation
```elisp
;; Set up timer that triggers after idle period
(run-with-idle-timer ecache-idle-invalidation-time t
                     #'ecache-clear-memory)
```

#### Hook-Based Invalidation
```elisp
;; Add function to hook that clears cache
(add-hook hook-name
          (lambda () (ecache-clear :backend backend)))
```

## API Design

### Unified API Pattern

The unified API (`ecache-set`, `ecache-get`, etc.) follows a consistent pattern:

```elisp
(ecache-operation key [value] &rest args)

;; Args are keyword arguments:
;; :backend 'memory|'file|'database
;; :ttl seconds
;; :default value
;; :buffer-local t|nil
```

This design is inspired by:
- **Browser Storage API**: Simple key-value operations
- **Redis API**: Support for TTL and multiple data types
- **Python's functools**: Transparent caching decorators

### Backend-Specific APIs

Each backend has direct functions for advanced use:
- `ecache-{backend}-set/get/delete/clear/keys`
- Useful when you know exactly which backend to use
- Slightly more efficient (no dispatch overhead)

## Performance Characteristics

### Operation Complexity

| Operation | Memory | File | Database |
|-----------|--------|------|----------|
| Set       | O(1)   | O(1) | O(log n) |
| Get       | O(1)   | O(1) | O(log n) |
| Delete    | O(1)   | O(1) | O(log n) |
| Clear     | O(n)   | O(n) | O(n)     |
| Keys      | O(n)   | O(n) | O(n)     |

### Memory Usage

- **Memory Cache**: ~100 bytes per entry (excluding value size)
- **File Cache**: ~1KB per file (filesystem overhead)
- **Database Cache**: ~100 bytes per row (plus SQLite overhead)

### Access Speed (relative)

1. **Memory**: 1x (baseline, ~1 microsecond)
2. **File**: ~1000x slower (disk I/O)
3. **Database**: ~100x slower (SQLite overhead)

## Cache Strategy Recommendations

### When to Use Each Backend

#### Memory Cache
- ✅ Frequently accessed data
- ✅ Temporary session data
- ✅ Small to medium datasets (<10,000 entries)
- ❌ Data that needs to persist across restarts
- ❌ Very large datasets

#### File Cache
- ✅ Persistent data needed across restarts
- ✅ Medium-sized datasets (<10,000 entries)
- ✅ Infrequently accessed data
- ❌ Very frequently accessed data (I/O overhead)
- ❌ Very large datasets (filesystem limits)

#### Database Cache
- ✅ Large datasets (>10,000 entries)
- ✅ Complex querying needs (future enhancement)
- ✅ Persistent data with ACID guarantees
- ❌ Requires Emacs 29+
- ❌ Simple key-value access (overhead not worth it)

#### Buffer-Local Cache
- ✅ Buffer-specific temporary data
- ✅ UI state for specific buffers
- ✅ Automatic cleanup when buffer is killed
- ❌ Data shared across buffers
- ❌ Persistent data

### Multi-Level Caching

For optimal performance, use a multi-level cache strategy:

```
Level 1: Memory (fast, temporary)
   ↓ miss
Level 2: File/Database (slower, persistent)
   ↓ miss
Level 3: Compute/Fetch (slowest)
```

Example implementation in `examples.el`: `example-multi-level-get`

## Design Principles

### 1. Simplicity
- Simple key-value API similar to browser localStorage
- Minimal configuration needed for basic use
- Sensible defaults

### 2. Flexibility
- Multiple backends for different use cases
- Extensible design (easy to add new backends)
- Fine-grained control when needed

### 3. Safety
- Automatic serialization prevents data corruption
- TTL prevents stale data
- LRU prevents memory exhaustion

### 4. Performance
- Lazy expiration (check on access, not on timer)
- Efficient hash table lookups
- Minimal overhead for common operations

## Future Enhancements

### Potential Features

1. **Distributed Cache**: Support for shared cache across multiple Emacs instances
2. **Compression**: Automatic compression for large values
3. **Encryption**: Optional encryption for sensitive data
4. **Statistics**: Detailed hit/miss ratios and performance metrics
5. **Query Support**: Complex queries on database cache
6. **Cache Warming**: Pre-populate cache on startup
7. **Namespaces**: Logical grouping of cache entries
8. **Backup/Restore**: Easy backup and restore of entire cache

### Extensibility Points

Adding a new backend requires implementing 5 functions:
1. `ecache-set-{backend}(key value &optional ttl)`
2. `ecache-get-{backend}(key &optional default)`
3. `ecache-delete-{backend}(key)`
4. `ecache-clear-{backend}()`
5. `ecache-keys-{backend}()`

## Comparison with Other Solutions

### vs Manual Hash Tables
- ✅ TTL support
- ✅ Persistence options
- ✅ LRU eviction
- ✅ Unified API
- ❌ Slight overhead

### vs savehist/saveplace
- ✅ More flexible (any data type)
- ✅ Multiple backends
- ✅ TTL support
- ✅ Programmatic API
- ❌ Not automatic for specific variables

### vs Custom File Storage
- ✅ Standardized format
- ✅ TTL and eviction
- ✅ Multiple backends
- ✅ Less boilerplate
- ❌ Opinionated design

## Testing Strategy

The test suite (`ecache-tests.el`) covers:

1. **Basic Operations**: Set, get, delete, clear
2. **Data Types**: Strings, numbers, lists, hash tables
3. **TTL**: Expiration behavior
4. **Isolation**: Buffer-local cache isolation
5. **Persistence**: File cache persistence
6. **API**: Unified API with different backends

## Contributing

When contributing to ecache:

1. Maintain backward compatibility
2. Add tests for new features
3. Update documentation
4. Follow existing code style
5. Consider performance implications

## License

GPL-3.0 or later
