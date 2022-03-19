#lang racket/base

(require pii_spider/settings
         rackunit
         mock
         mock/rackunit)

(provide settings-tests)

(define settings-tests
  (test-suite
   "settings"
   (test-suite
    "default-settings" 
    (test-case "returns a hash"
      (check-true (hash? (default-settings))))
    (test-case "returns a default for server of localhost"
      (check-equal? (hash-ref (default-settings) 'server) "localhost"))
    (test-case "returns a default for port of 5432"
      (check-equal? (hash-ref (default-settings) 'port) 5432))
    (test-case "returns a default for ignoreFile of ignore.json"
      (check-equal? (hash-ref (default-settings) 'ignoreFile) "ignore.json")))
   (test-suite
    "add-environment-vars"
    (test-case "returns the settings hash"
      (define test-settings (make-hash))
      (hash-set! test-settings 'ignoreFile "ignore.json")
      (check-equal? (hash-ref (add-environment-vars test-settings) 'ignoreFile) "ignore.json"))
    (test-case "sets the value of server the env var PII_SPIDER_SERVER"
      (define test-settings (make-hash))
      (hash-set! test-settings 'server "localhost")
      (putenv "PII_SPIDER_SERVER" "db.test.com")
      (check-equal? (hash-ref (add-environment-vars test-settings) 'server) "db.test.com" ))
    (test-case "does not overide the default value of server if PII_SPIDER_SERVER isn't set"
      (define test-settings (make-hash))
      (hash-set! test-settings 'server "localhost")
      (environment-variables-set! (current-environment-variables)
                                  (string->bytes/utf-8 "PII_SPIDER_SERVER") #f)
      (check-equal? (hash-ref (add-environment-vars test-settings) 'server) "localhost" ))
    (test-case "does not provide a value for password if PII_SPIDER_PASSWORD isn't set"
      (define test-settings (make-hash))
      (environment-variables-set! (current-environment-variables)
                                  (string->bytes/utf-8 "PII_SPIDER_PASSWORD") #f)
      (check-true (hash-ref (add-environment-vars test-settings) 'password #t))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests settings-tests))
