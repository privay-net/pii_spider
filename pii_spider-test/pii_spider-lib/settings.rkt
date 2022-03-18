#lang racket/base

(require pii_spider/settings
         rackunit)

(provide settings-tests)

(define settings-tests
  (test-suite
   "settings"
   (test-case "default-settings returns a hash"
     (check-true (hash? (default-settings))))
   (test-case "default-settings returns a default for server of localhost"
     (check-equal? (hash-ref (default-settings) 'server) "localhost"))
   (test-case "default-settings returns a default for port of 5432"
     (check-equal? (hash-ref (default-settings) 'port) 5432))
   (test-case "default-settings returns a default for ignoreFile of ignore.json"
     (check-equal? (hash-ref (default-settings) 'ignoreFile) "ignore.json"))))

(module+ test
  (require rackunit/text-ui)
  (run-tests settings-tests))
