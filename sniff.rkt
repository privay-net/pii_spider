#lang racket

(require db)

(provide sniffer)
(define (sniffer url #:connector [initialise-connection initialise-connection]
                     #:list-tables [list-tables list-tables])
  ; connect to the db
  (define pgc (initialise-connection))

  ; find all the tables
  (list-tables pgc)
  ; deal with taking some small number of rows vs scanning the entire thing
  ; check for pii_data in each table
  ; return report
  url
  )

(define (initialise-connection #:connector [postgresql-connect postgresql-connect])
      (postgresql-connect #:user "robert"
                        #:database "pii"
                        #:password "bhujasample4$"))

(define (examine-table table-name connection #:query-function [query-rows query-rows])

  (let ([query (string-append "select * from " table-name ";")])
    ;get rows
    (query-rows connection query))
  ; look in each row for pii data
  ; return pii rows
  )

(module+ test
  (require rackunit)
  (require mock)
  (require mock/rackunit)

  (define-opaque test-connection)
  (define connector-mock (mock #:behavior (const test-connection)))
  (define-opaque test-list-tables)
  (define list-tables-mock (mock #:behavior (const test-list-tables)))
    
  
  ; TODO make this a big old omnibus test
  (test-case "sniffer returns a report structure"
    (check-eq?
     (sniffer "not://a.url/test"
              #:connector connector-mock
              #:list-tables list-tables-mock) "not://a.url/test"))

  (test-case "sniffer compiles a list of tables to examine"
    (sniffer "not://a.url/test" #:connector connector-mock #:list-tables list-tables-mock)
    (check-mock-called-with? list-tables-mock (arguments test-connection)))

  (test-case "initialise-connection gets the connection for a database"
    (initialise-connection #:connector connector-mock)
    (check-mock-called-with? connector-mock (arguments #:database "pii"
                                                       #:password "bhujasample4$"
                                                       #:user "robert")))
  
  (test-case "examine-table gets the rows from the table"
    (define query-mock (mock #:behavior (const test-connection)))
    (examine-table "foo" connector-mock #:query-function query-mock)
    (check-mock-called-with? query-mock (arguments connector-mock "select * from foo;"))))

; TODO deal with table not existing error
