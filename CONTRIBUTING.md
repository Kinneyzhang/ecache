# Contributing to ecache

Thank you for your interest in contributing to ecache! This document provides guidelines and instructions for contributing.

## Getting Started

1. Fork the repository
2. Clone your fork: `git clone https://github.com/YOUR-USERNAME/ecache.git`
3. Create a feature branch: `git checkout -b feature/your-feature-name`
4. Make your changes
5. Run tests (see Testing section)
6. Commit your changes: `git commit -m "Description of changes"`
7. Push to your fork: `git push origin feature/your-feature-name`
8. Open a Pull Request

## Development Setup

### Requirements

- Emacs 26.1 or later (for basic features)
- Emacs 29.1 or later (for database cache support)

### Loading the Package

```elisp
;; Add to your Emacs configuration
(add-to-list 'load-path "/path/to/ecache")
(require 'ecache)
(ecache-init)
```

## Code Style

### General Guidelines

1. **Use `lexical-binding`**: All files should have `; -*- lexical-binding: t; -*-`
2. **Follow Emacs Lisp conventions**: Use standard naming conventions (e.g., `package-name--private-function`)
3. **Document functions**: All public functions should have docstrings
4. **Keep functions focused**: Each function should do one thing well
5. **Use descriptive names**: Variable and function names should be clear and descriptive

### Naming Conventions

- **Public API functions**: `ecache-operation` (e.g., `ecache-set`, `ecache-get`)
- **Backend-specific functions**: `ecache-operation-backend` (e.g., `ecache-set-memory`)
- **Internal functions**: `ecache--internal-function` (double dash for private)
- **Variables**: `ecache-variable-name`
- **Internal variables**: `ecache--internal-variable`

### Example

```elisp
(defun ecache-set-memory (key value &optional ttl)
  "Set KEY to VALUE in global memory cache with optional TTL.

KEY is a string representing the cache key.
VALUE can be any serializable Lisp object.
TTL is the time to live in seconds (optional).

Example:
  (ecache-set-memory \"my-key\" \"my-value\" 60)"
  (ecache--evict-lru)
  (puthash key value ecache--global-cache)
  (puthash key (list :created (ecache--current-time)
                     :accessed (ecache--current-time)
                     :ttl ttl)
           ecache--cache-metadata)
  (ecache--update-access key)
  value)
```

## Testing

### Running Tests

Tests are located in `ecache-tests.el`. To run tests:

```bash
# Run all tests
emacs --batch -l ecache.el -l ecache-tests.el -f ert-run-tests-batch-and-exit

# Run specific test
emacs --batch -l ecache.el -l ecache-tests.el --eval "(ert-run-tests-batch-and-exit 'ecache-test-memory-basic)"
```

### Writing Tests

When adding new features, please include tests. Use the `ert` framework:

```elisp
(ert-deftest ecache-test-your-feature ()
  "Test description."
  (ecache-clear-memory)
  (ecache-set-memory "test-key" "test-value")
  (should (equal "test-value" (ecache-get-memory "test-key"))))
```

### Test Coverage

Tests should cover:
- Basic functionality (set, get, delete, clear)
- Edge cases (nil values, special characters in keys)
- Error conditions (invalid inputs, expired TTLs)
- Different data types (strings, numbers, lists, hash tables)
- Backend-specific behavior

## Adding New Features

### Adding a New Backend

To add a new cache backend (e.g., Redis, HTTP):

1. Implement the five core functions:
```elisp
(defun ecache-set-yourbackend (key value &optional ttl)
  "Set KEY to VALUE in your backend.")

(defun ecache-get-yourbackend (key &optional default)
  "Get value for KEY from your backend.")

(defun ecache-delete-yourbackend (key)
  "Delete KEY from your backend.")

(defun ecache-clear-yourbackend ()
  "Clear all entries in your backend.")

(defun ecache-keys-yourbackend ()
  "Return list of all keys in your backend.")
```

2. Update the unified API to support your backend:
```elisp
;; In ecache-set, ecache-get, etc.
((eq backend 'yourbackend)
 (ecache-set-yourbackend key value ttl))
```

3. Add tests for your backend:
```elisp
(ert-deftest ecache-test-yourbackend-basic ()
  "Test basic operations for your backend."
  ...)
```

4. Document your backend in README.md and ARCHITECTURE.md

### Adding New Invalidation Strategies

To add a new invalidation strategy:

1. Create a setup function:
```elisp
(defun ecache-invalidate-on-yourstrategy ()
  "Set up your invalidation strategy."
  (add-hook 'your-hook #'ecache-clear-memory))
```

2. Add documentation and examples
3. Add tests

## Documentation

### Docstrings

All public functions must have docstrings that include:
- Brief description
- Parameter descriptions
- Return value description
- Usage examples (optional but recommended)

Example:
```elisp
(defun ecache-set (key value &rest args)
  "Set KEY to VALUE in cache.

KEY is a string representing the cache key.
VALUE can be any serializable Lisp object.

ARGS is a plist with the following optional keys:
  :backend   - Cache backend: 'memory (default), 'file, or 'database
  :ttl       - Time to live in seconds
  :buffer-local - If non-nil, use buffer-local cache (overrides :backend)

Returns VALUE.

Examples:
  (ecache-set \"key\" \"value\")
  (ecache-set \"key\" \"value\" :ttl 60)
  (ecache-set \"key\" \"value\" :backend 'file)"
  ...)
```

### README Updates

When adding features, update:
- README.md (English)
- README-CN.md (Chinese)
- ARCHITECTURE.md (technical details)
- examples.el (usage examples)

## Pull Request Guidelines

### Before Submitting

- [ ] Code follows style guidelines
- [ ] All tests pass
- [ ] New features have tests
- [ ] Documentation is updated
- [ ] Commit messages are clear and descriptive
- [ ] No unnecessary files (build artifacts, temp files)

### PR Description

Include in your PR description:
- What the PR does
- Why the change is needed
- How to test the changes
- Any breaking changes
- Screenshots (if UI changes)

### Example PR Description

```markdown
## Summary
Add support for Redis backend cache

## Motivation
Allow using Redis as a cache backend for distributed caching scenarios.

## Changes
- Added `ecache-set-redis`, `ecache-get-redis`, etc.
- Updated unified API to support `:backend 'redis`
- Added tests for Redis backend
- Updated documentation

## Testing
1. Start Redis server: `redis-server`
2. Run tests: `emacs --batch -l ecache.el -l ecache-tests.el -f ert-run-tests-batch-and-exit`

## Breaking Changes
None

## Dependencies
Requires `redis.el` package
```

## Reporting Issues

### Bug Reports

When reporting bugs, include:
- Emacs version: `M-x emacs-version`
- ecache version
- Steps to reproduce
- Expected behavior
- Actual behavior
- Error messages (if any)

### Feature Requests

When requesting features:
- Describe the use case
- Explain why it's useful
- Suggest implementation approach (optional)
- Provide examples

## Code Review Process

1. Maintainer reviews the PR
2. Feedback is provided (if needed)
3. Author addresses feedback
4. Maintainer approves and merges

## License

By contributing, you agree that your contributions will be licensed under the GPL-3.0 License.

## Questions?

If you have questions:
- Open an issue
- Check existing documentation
- Look at examples in `examples.el`

## Thanks!

Thank you for contributing to ecache! Your efforts help make it better for everyone.
