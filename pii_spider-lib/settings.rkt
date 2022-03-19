#lang racket/base

(require racket/hash)

(provide default-settings add-environment-vars)

(define (default-settings)
  (define result (make-hash))
  (hash-set! result 'server "localhost")
  (hash-set! result 'port  5432)
  (hash-set! result 'ignoreFile  "ignore.json")
  result)

(define (add-environment-vars settings)
  (define env-settings (make-hash))
  (hash-set! env-settings 'server (getenv "PII_SPIDER_SERVER"))
  (hash-set! env-settings 'port (getenv "PII_SPIDER_PORT"))
  (hash-set! env-settings 'ignore-file (getenv "PII_SPIDER_IGNOREFILE"))
  (hash-set! env-settings 'database (getenv "PII_SPIDER_DATABASE"))
  (hash-set! env-settings 'username (getenv "PII_SPIDER_USERNAME"))
  (hash-set! env-settings 'password (getenv "PII_SPIDER_PASSWORD"))
  
  (hash-map env-settings (lambda (key val) (unless val
                                             (hash-remove! env-settings key))))
  (hash-union! settings env-settings #:combine/key (lambda (k v1 v2) v2))
  settings)
