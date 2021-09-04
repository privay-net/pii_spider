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
  url)

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
    (check-mock-called-with? list-tables-mock (arguments test-connection))))


(define (initialise-connection #:connector [postgresql-connect postgresql-connect])
  (postgresql-connect #:user "robert"
                      #:database "pii"
                      #:password "bhujasample4$"))

(module+ test
  ;;; TODO deal with a failed connection
  ;;;  TODO pool connections
  (test-case "initialise-connection gets the connection for a database"
    (initialise-connection #:connector connector-mock)
    (check-mock-called-with? connector-mock (arguments #:database "pii"
                                                       #:password "bhujasample4$"
                                                       #:user "robert"))))


(define (examine-table connection table-name #:query-function [query-rows query-rows])
  (let ([query (string-append "select * from \"" table-name "\";")])
    (query-rows connection query)))

(module+ test
  ;;; TODO deal with table not existing error  
  (test-case "examine-table exectutes a query with the requiired arguments"
    (define query-mock (mock #:behavior (const test-connection)))
    (examine-table connector-mock "foo" #:query-function query-mock)
    (check-mock-called-with? query-mock (arguments connector-mock "select * from \"foo\";")))
  
  (test-case "examine-table returns a list of rows"
    (define one-row-result '(#(1 "user@example.com")))
    (define query-mock (mock #:behavior (const one-row-result)))
    (check-eq? (examine-table connector-mock "foo" #:query-function query-mock) one-row-result)))


(define (sniff-for-pii row rule)
  (vector-map rule row))

(module+ test
  (define row-result #(1 "user@example.com"))
  
  (test-case "sniff-for-pii run rules over each row looking for PII"
    (define-opaque test-rule)
    (define rule-mock (mock #:behavior (const test-rule)))
    (sniff-for-pii row-result rule-mock)
    (check-mock-called-with? rule-mock (arguments (vector-ref row-result 0)))
    (check-mock-called-with? rule-mock (arguments (vector-ref row-result 1))))

  (test-case "sniff-for-pii returns a count of the PII instances detected")
  (test-case "sniff-for-pii returns the name of the rule when PII is detected")
  (test-case "sniff-for-pii returns an count of 0 when no PII is detected")
  (test-case "sniff-for-pii returns the rule name when no  PII instances detected"))





