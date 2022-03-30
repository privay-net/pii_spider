#lang racket/base
(require pii_spider/structs)

(provide raise-db-connection-error)

(define (raise-db-connection-error settings)
  (log-debug (format "DB Connection Settings: \n ~a" settings))
  (raise (exn:fail:pii-spider:db-connection
          (format "Could not connect to: database")
          (current-continuation-marks))))
