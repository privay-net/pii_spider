#lang racket

(require racket/struct)

(provide examined-row examined-row-id examined-row-results)

(struct examined-row (id results)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'examined-row)
      (lambda (obj) (list (examined-row-id obj) (examined-row-results obj)))))]) 
