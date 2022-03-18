#lang racket/base

(module+ test
  (require rackunit))

(provide default-settings add-environment-vars)

(define (default-settings)
  (define result (make-hash))
  (hash-set! result 'server "localhost")
  (hash-set! result 'port  5432)
  (hash-set! result 'ignoreFile  "ignore.json")
  result)

(module+ test
  (test-case "default-settings returns a hash"
    (check-true (hash? (default-settings))))
  (test-case "default-settings returns a default for server of localhost"
    (check-equal? (hash-ref (default-settings) 'server) "localhost"))
  (test-case "default-settings returns a default for port of 5432"
    (check-equal? (hash-ref (default-settings) 'port) 5432))
  (test-case "default-settings returns a default for ignoreFile of ignore.json"
    (check-equal? (hash-ref (default-settings) 'ignoreFile) "ignore.json")))

(define add-environment-vars #t)
