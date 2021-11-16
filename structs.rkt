#lang racket

(require racket/struct)

(provide examined-row examined-row-id examined-row-results
         examined-table examined-table? examined-table-name examined-table-row-count)

(struct examined-row (id results)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'examined-row)
      (lambda (obj) (list (examined-row-id obj) (examined-row-results obj)))))]) 

(struct examined-table (name start-time end-time row-count results)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'examined-table)
      (lambda (obj) (list (examined-table-name obj)
                          (examined-table-row-count obj)
                          (examined-table-end-time)))))]) 
