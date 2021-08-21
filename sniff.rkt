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
  ; look in each row for pii data
  ; return pii rows

  ; return report
  url
  )

(define (initialise-connection #:connector [postgresql-connect postgresql-connect])
      (postgresql-connect #:user "robert"
                        #:database "pii"
                        #:password "bhujasample4$"))

(define (examine-table connection table-name #:query-function [query-rows query-rows])
  (let ([query (string-append "select * from \"" table-name "\";")])
    (query-rows connection query)))

(module+ test
  (require rackunit)
  (require mock)
  (require mock/rackunit)

  (define-opaque test-connection)
  (define connector-mock (mock #:behavior (const test-connection)))
  (define-opaque test-list-tables)
  (define list-tables-mock (mock #:behavior (const test-list-tables)))
  
  
  ;;; TODO make this a big old omnibus test
  (test-case "sniffer returns a report structure"
    (check-eq?
     (sniffer "not://a.url/test"
              #:connector connector-mock
              #:list-tables list-tables-mock) "not://a.url/test"))
  
  (test-case "sniffer compiles a list of tables to examine"
    (sniffer "not://a.url/test" #:connector connector-mock #:list-tables list-tables-mock)
    (check-mock-called-with? list-tables-mock (arguments test-connection)))

  ;;; TODO deal with a failed connection
  (test-case "initialise-connection gets the connection for a database"
    (initialise-connection #:connector connector-mock)
    (check-mock-called-with? connector-mock (arguments #:database "pii"
                                                       #:password "bhujasample4$"
                                                       #:user "robert")))

  ;;; TODO deal with table not existing error  
  (test-case "examine-table exectutes a query with the requiired arguments"
    (define query-mock (mock #:behavior (const test-connection)))
    (examine-table connector-mock "foo" #:query-function query-mock)
    (check-mock-called-with? query-mock (arguments connector-mock "select * from \"foo\";")))
  
  (test-case "examine-table returns a list of rows"
    (define row-result '(#(1 "user@example.com")))
    (define query-mock (mock #:behavior (const row-result)))
    (check-eq? (examine-table connector-mock "foo" #:query-function query-mock) row-result)))





