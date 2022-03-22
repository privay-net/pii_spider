#lang racket/base

(require racket/struct)

(provide (struct-out examined-row) (struct-out examined-table) (struct-out ignore)
         (struct-out exn:fail:pii-spider) (struct-out exn:fail:pii-spider:db-connection))

(struct examined-row (id results)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'examined-row)
      (lambda (obj) (list (examined-row-id obj) (examined-row-results obj)))))]) 

(struct examined-table (name start-time end-time row-count results ignored)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'examined-table)
      (lambda (obj) (list (examined-table-name obj)
                          (examined-table-row-count obj)
                          (examined-table-end-time obj)
                          (examined-table-ignored obj)))))]) 
(struct ignore (tables columns rows)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'ignores)
      (lambda (obj) (list (ignore-tables obj)
                          (ignore-columns obj)
                          (ignore-rows obj)))))])

(struct exn:fail:pii-spider exn:fail ())
(struct exn:fail:pii-spider:db-connection exn:fail:pii-spider ())
