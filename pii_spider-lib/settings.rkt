#lang racket/base

(provide default-settings add-environment-vars)

(define (default-settings)
  (define result (make-hash))
  (hash-set! result 'server "localhost")
  (hash-set! result 'port  5432)
  (hash-set! result 'ignoreFile  "ignore.json")
  result)

(define add-environment-vars #t)
