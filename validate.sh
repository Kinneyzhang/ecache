#!/bin/bash
# Manual validation script for ecache
# This script can be run in an Emacs environment to test basic functionality

cat << 'EOFDEMO' > /tmp/ecache-demo.el
;;; Demo script to validate ecache functionality

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'ecache)

(message "\n=== Starting ecache validation ===\n")

;; Initialize
(ecache-init)
(message "✓ Initialized ecache")

;; Test 1: Memory cache basic operations
(ecache-clear-memory)
(ecache-set-memory "test-key" "test-value")
(let ((result (ecache-get-memory "test-key")))
  (if (equal result "test-value")
      (message "✓ Memory cache: basic set/get works")
    (error "✗ Memory cache: basic set/get failed")))

;; Test 2: Memory cache with different types
(ecache-set-memory "number" 42)
(ecache-set-memory "list" '(1 2 3))
(let ((num (ecache-get-memory "number"))
      (lst (ecache-get-memory "list")))
  (if (and (equal num 42) (equal lst '(1 2 3)))
      (message "✓ Memory cache: multiple data types work")
    (error "✗ Memory cache: data type test failed")))

;; Test 3: Unified API
(ecache-set "unified-key" "unified-value")
(let ((result (ecache-get "unified-key")))
  (if (equal result "unified-value")
      (message "✓ Unified API: works correctly")
    (error "✗ Unified API: failed")))

;; Test 4: Default values
(let ((result (ecache-get "nonexistent" :default "default-value")))
  (if (equal result "default-value")
      (message "✓ Default values: work correctly")
    (error "✗ Default values: failed")))

;; Test 5: Delete operation
(ecache-set-memory "delete-test" "value")
(ecache-delete-memory "delete-test")
(let ((result (ecache-get-memory "delete-test")))
  (if (null result)
      (message "✓ Delete operation: works correctly")
    (error "✗ Delete operation: failed")))

;; Test 6: Keys listing
(ecache-clear-memory)
(ecache-set-memory "key1" "value1")
(ecache-set-memory "key2" "value2")
(let ((keys (ecache-keys-memory)))
  (if (= (length keys) 2)
      (message "✓ Keys listing: works correctly (found %d keys)" (length keys))
    (error "✗ Keys listing: failed")))

;; Test 7: File cache
(ecache-clear-file)
(ecache-set-file "file-key" "file-value")
(let ((result (ecache-get-file "file-key")))
  (if (equal result "file-value")
      (message "✓ File cache: basic operations work")
    (error "✗ File cache: failed")))

;; Test 8: Clear operations
(ecache-clear-memory)
(let ((keys (ecache-keys-memory)))
  (if (= (length keys) 0)
      (message "✓ Clear operation: works correctly")
    (error "✗ Clear operation: failed")))

;; Test 9: Buffer-local cache
(with-temp-buffer
  (ecache-set-buffer-local "buffer-key" "buffer-value")
  (let ((result (ecache-get-buffer-local "buffer-key")))
    (if (equal result "buffer-value")
        (message "✓ Buffer-local cache: works correctly")
      (error "✗ Buffer-local cache: failed"))))

;; Test 10: Stats
(ecache-clear :backend 'all)
(ecache-set "test1" "value1")
(ecache-set "test2" "value2" :backend 'file)
(message "✓ Statistics: Memory=%d File=%d" 
         (length (ecache-keys-memory))
         (length (ecache-keys-file)))

(message "\n=== All validation tests passed! ===\n")
(message "ecache is working correctly.")
EOFDEMO

echo "Validation script created at /tmp/ecache-demo.el"
echo ""
echo "To run this validation, execute:"
echo "  emacs --batch -l /tmp/ecache-demo.el"
echo ""
echo "Or in an interactive Emacs session:"
echo "  M-x load-file RET /tmp/ecache-demo.el RET"
echo ""
echo "The script tests:"
echo "  - Memory cache operations"
echo "  - Multiple data types"
echo "  - Unified API"
echo "  - Default values"
echo "  - Delete operations"
echo "  - Keys listing"
echo "  - File cache"
echo "  - Clear operations"
echo "  - Buffer-local cache"
echo "  - Statistics"
