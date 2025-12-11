;;; ecache-tests.el --- Tests for ecache -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Tests for the ecache package.

;;; Code:

(require 'ert)
(require 'ecache)

;;; Memory Cache Tests

(ert-deftest ecache-test-memory-basic ()
  "Test basic memory cache operations."
  (ecache-clear-memory)
  (should (null (ecache-get-memory "test-key")))
  (should (equal "value" (ecache-set-memory "test-key" "value")))
  (should (equal "value" (ecache-get-memory "test-key")))
  (ecache-delete-memory "test-key")
  (should (null (ecache-get-memory "test-key"))))

(ert-deftest ecache-test-memory-default ()
  "Test memory cache with default value."
  (ecache-clear-memory)
  (should (equal "default" (ecache-get-memory "nonexistent" "default"))))

(ert-deftest ecache-test-memory-ttl ()
  "Test memory cache with TTL."
  (ecache-clear-memory)
  (ecache-set-memory "ttl-key" "value" 1)
  (should (equal "value" (ecache-get-memory "ttl-key")))
  (sleep-for 2)
  (should (null (ecache-get-memory "ttl-key"))))

(ert-deftest ecache-test-memory-types ()
  "Test memory cache with different data types."
  (ecache-clear-memory)
  (ecache-set-memory "string" "text")
  (ecache-set-memory "number" 42)
  (ecache-set-memory "list" '(1 2 3))
  (ecache-set-memory "hash" (let ((h (make-hash-table)))
                               (puthash "key" "value" h)
                               h))
  (should (equal "text" (ecache-get-memory "string")))
  (should (equal 42 (ecache-get-memory "number")))
  (should (equal '(1 2 3) (ecache-get-memory "list")))
  (should (hash-table-p (ecache-get-memory "hash"))))

(ert-deftest ecache-test-memory-keys ()
  "Test listing memory cache keys."
  (ecache-clear-memory)
  (ecache-set-memory "key1" "value1")
  (ecache-set-memory "key2" "value2")
  (let ((keys (ecache-keys-memory)))
    (should (member "key1" keys))
    (should (member "key2" keys))
    (should (= 2 (length keys)))))

(ert-deftest ecache-test-memory-clear ()
  "Test clearing memory cache."
  (ecache-clear-memory)
  (ecache-set-memory "key1" "value1")
  (ecache-set-memory "key2" "value2")
  (should (= 2 (length (ecache-keys-memory))))
  (ecache-clear-memory)
  (should (= 0 (length (ecache-keys-memory)))))

;;; Buffer-Local Cache Tests

(ert-deftest ecache-test-buffer-local-basic ()
  "Test basic buffer-local cache operations."
  (with-temp-buffer
    (should (null (ecache-get-buffer-local "test-key")))
    (should (equal "value" (ecache-set-buffer-local "test-key" "value")))
    (should (equal "value" (ecache-get-buffer-local "test-key")))
    (ecache-delete-buffer-local "test-key")
    (should (null (ecache-get-buffer-local "test-key")))))

(ert-deftest ecache-test-buffer-local-isolation ()
  "Test that buffer-local caches are isolated."
  (with-temp-buffer
    (ecache-set-buffer-local "key" "buffer1")
    (should (equal "buffer1" (ecache-get-buffer-local "key")))
    (with-temp-buffer
      (should (null (ecache-get-buffer-local "key")))
      (ecache-set-buffer-local "key" "buffer2")
      (should (equal "buffer2" (ecache-get-buffer-local "key"))))))

(ert-deftest ecache-test-buffer-local-clear ()
  "Test clearing buffer-local cache."
  (with-temp-buffer
    (ecache-set-buffer-local "key1" "value1")
    (ecache-set-buffer-local "key2" "value2")
    (should (= 2 (length (ecache-keys-buffer-local))))
    (ecache-clear-buffer-local)
    (should (= 0 (length (ecache-keys-buffer-local))))))

;;; File Cache Tests

(ert-deftest ecache-test-file-basic ()
  "Test basic file cache operations."
  (ecache-clear-file)
  (should (null (ecache-get-file "test-key")))
  (should (equal "value" (ecache-set-file "test-key" "value")))
  (should (equal "value" (ecache-get-file "test-key")))
  (ecache-delete-file "test-key")
  (should (null (ecache-get-file "test-key"))))

(ert-deftest ecache-test-file-persistence ()
  "Test that file cache persists."
  (ecache-clear-file)
  (ecache-set-file "persist-key" "persist-value")
  (should (equal "persist-value" (ecache-get-file "persist-key")))
  ;; Simulate restart by just reading again
  (should (equal "persist-value" (ecache-get-file "persist-key"))))

(ert-deftest ecache-test-file-ttl ()
  "Test file cache with TTL."
  (ecache-clear-file)
  (ecache-set-file "ttl-key" "value" 1)
  (should (equal "value" (ecache-get-file "ttl-key")))
  (sleep-for 2)
  (should (null (ecache-get-file "ttl-key"))))

(ert-deftest ecache-test-file-types ()
  "Test file cache with different data types."
  (ecache-clear-file)
  (ecache-set-file "string" "text")
  (ecache-set-file "number" 42)
  (ecache-set-file "list" '(1 2 3))
  (should (equal "text" (ecache-get-file "string")))
  (should (equal 42 (ecache-get-file "number")))
  (should (equal '(1 2 3) (ecache-get-file "list"))))

;;; Unified API Tests

(ert-deftest ecache-test-unified-api-memory ()
  "Test unified API with memory backend."
  (ecache-clear :backend 'memory)
  (ecache-set "test-key" "value")
  (should (equal "value" (ecache-get "test-key")))
  (ecache-delete "test-key")
  (should (null (ecache-get "test-key"))))

(ert-deftest ecache-test-unified-api-file ()
  "Test unified API with file backend."
  (ecache-clear :backend 'file)
  (ecache-set "test-key" "value" :backend 'file)
  (should (equal "value" (ecache-get "test-key" :backend 'file)))
  (ecache-delete "test-key" :backend 'file)
  (should (null (ecache-get "test-key" :backend 'file))))

(ert-deftest ecache-test-unified-api-buffer-local ()
  "Test unified API with buffer-local cache."
  (with-temp-buffer
    (ecache-set "test-key" "value" :buffer-local t)
    (should (equal "value" (ecache-get "test-key" :buffer-local t)))
    (ecache-delete "test-key" :buffer-local t)
    (should (null (ecache-get "test-key" :buffer-local t)))))

(ert-deftest ecache-test-unified-api-ttl ()
  "Test unified API with TTL."
  (ecache-clear :backend 'memory)
  (ecache-set "ttl-key" "value" :ttl 1)
  (should (equal "value" (ecache-get "ttl-key")))
  (sleep-for 2)
  (should (null (ecache-get "ttl-key"))))

(ert-deftest ecache-test-unified-api-default ()
  "Test unified API with default value."
  (ecache-clear :backend 'memory)
  (should (equal "default" (ecache-get "nonexistent" :default "default"))))

;;; Save/Load Tests

(ert-deftest ecache-test-save-load ()
  "Test saving and loading memory cache."
  (ecache-clear-memory)
  (ecache-set-memory "key1" "value1")
  (ecache-set-memory "key2" "value2")
  (let ((temp-file (make-temp-file "ecache-test")))
    (unwind-protect
        (progn
          (ecache-save-to-file temp-file)
          (ecache-clear-memory)
          (should (= 0 (length (ecache-keys-memory))))
          (ecache-load-from-file temp-file)
          (should (equal "value1" (ecache-get-memory "key1")))
          (should (equal "value2" (ecache-get-memory "key2"))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(provide 'ecache-tests)

;;; ecache-tests.el ends here
