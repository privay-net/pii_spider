#lang racket/base
(require pii_spider/structs)

(provide raise-db-connection-error)

(define (raise-db-connection-error database)
  (raise (exn:fail:pii-spider:db-connection
          (format "Could not connect to: ~a" database)
          (current-continuation-marks))))
