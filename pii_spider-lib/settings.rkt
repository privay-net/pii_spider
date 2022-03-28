#lang racket/base

(require racket/hash)

(provide default-settings add-environment-vars)

(define (default-settings)
  (define result (make-hash))
  (hash-set! result 'server "localhost")
  (hash-set! result 'port  5432)
  (hash-set! result 'ignoreFile  "ignore.json")
  (hash-set! result 'daemon #f)
  (define output-dir (build-path (current-directory) "output"))
  (log-debug (format "Default output directory is ~a" (path->string output-dir)))
  (hash-set! result 'outputDir output-dir)
  result)

(define (add-environment-vars settings)
  (define env-settings (make-hash))
  (define env-var-mappings (hash 'server "PII_SPIDER_SERVER"
                                 'port "PII_SPIDER_PORT"
                                 'ignoreFile "PII_SPIDER_IGNOREFILE"
                                 'database "PII_SPIDER_DATABASE"
                                 'username "PII_SPIDER_USERNAME"
                                 'password "PII_SPIDER_PASSWORD"
                                 'outputDir "PII_SPIDER_OUTPUTDIR"
                                 'daemon "PII_SPIDER_DAEMON"))
  (hash-map env-var-mappings (lambda (key val)
                               (hash-set! env-settings key (getenv val))))
  
  (hash-map env-settings (lambda (key val) (unless val
                                             (hash-remove! env-settings key))))
  (hash-union! settings env-settings #:combine/key (lambda (k v1 v2) v2))
  settings)
