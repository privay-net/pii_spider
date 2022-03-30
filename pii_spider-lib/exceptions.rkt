#lang racket/base
(require pii_spider/structs)
(require "logging.rkt")

(provide raise-db-connection-error)

(define (raise-db-connection-error settings)
  (log-agent-debug (format "DB Connection Settings: \n ~a" settings))
  (raise (exn:fail:pii-spider:db-connection
          (format "Could not connect to: database")
          (current-continuation-marks))))
