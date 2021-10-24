#lang racket

(require db)
(require "examined-rows.rkt")
(require (prefix-in rules: "pii/rules.rkt"))

(provide sniffer)
(define (sniffer url #:connector [initialise-connection initialise-connection]
                 #:list-tables [list-tables list-tables])
  ; connect to the db
  (define pgc (initialise-connection))

  ; find all the tables
  (list-tables pgc)
  ; deal with taking some small number of rows vs scanning the entire thing
  ; grab rows of data from each table
  ;; (define rows (examine-table pgc "users"))
  ; have a set of rules applied to each set of table rows
  (define rules (list rules:email rules:au-phone-number))
  ; look in each row for pii data
  ; return pii rows
  ;; (examine-rows rows rules)
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
  ;;; TODO make the connection dynamic from env vars
  ;;; TODO deal with a failed connection
  ;;; TODO pool connections
  (test-case "initialise-connection gets the connection for a database"
    (initialise-connection #:connector connector-mock)
    (check-mock-called-with? connector-mock (arguments #:database "pii"
                                                       #:password "bhujasample4$"
                                                       #:user "robert"))))

(define (examine-table connection table-name #:query-function [query-rows query-rows])
  ;; use a better query and detect the primary key
  (let ([query (string-append "select * from \"" table-name "\";")])
    (query-rows connection query)))

(module+ test
  ;;; TODO deal with table not existing error  
  (test-case "examine-table exectutes a query with the required arguments"
    (define query-mock (mock #:behavior (const test-connection)))
    (examine-table connector-mock "foo" #:query-function query-mock)
    (check-mock-called-with? query-mock (arguments connector-mock "select * from \"foo\";")))
  
  (test-case "examine-table returns a list of rows"
    (define one-row-result '(#(1 "user@example.com")))
    (define query-mock (mock #:behavior (const one-row-result)))
    (check-eq? (examine-table connector-mock "foo" #:query-function query-mock) one-row-result)))

(define (examine-rows rows rules #:examiner-function [sniff-for-pii sniff-for-pii])
  (map (lambda (row)
         (map (lambda (rule)
                (examined-row (extract-primary-key row)
                              (sniff-for-pii row rule)))
              rules)) rows))

(module+ test
  (test-case "examine-rows applies sniff-for-pii to each row and rule"
    (define sniff-for-pii-mock (mock #:behavior (const (void)) ))
    (define rows '(#(1 "robert@test.com" "0412345678" "Robert")
                   #(2 "rob@test.com" "0412345679" "Rob")))
    (define rule1-mock (mock #:behavior (const (void)) ))
    (define rule2-mock (mock #:behavior (const (void)) ))
    (define rules (list rule1-mock rule2-mock))
    (examine-rows rows rules #:examiner-function sniff-for-pii-mock)
    (check-mock-called-with? sniff-for-pii-mock (arguments (car rows) (car rules)))
    (check-mock-called-with? sniff-for-pii-mock (arguments (cadr rows) (car rules)))
    (check-mock-called-with? sniff-for-pii-mock (arguments (car rows) (cadr rules)))
    (check-mock-called-with? sniff-for-pii-mock (arguments (cadr rows) (cadr rules)))))

(define (extract-primary-key row [primary-key-locations '(0)])
  (list "key" 
        (map  (lambda (location) (vector-ref row location)) primary-key-locations)))

(module+ test
  (test-case "extract-primary-key returns the value of the first column as the default primary key"
    (define primary-key 1)
    (define row (vector primary-key "robert@test.com" "0412345678" "Robert"))
    (check-equal? (extract-primary-key row) (list "key" (list primary-key))))
  (test-case "extract-primary-key returns a list of values as the primary key"
    (define target-keys '(0 1 2))
    (define key-fields '(1 "robert@test.com" "0412345678"))
    (define row (list->vector (append key-fields '("Robert"))))
    (check-equal? (extract-primary-key row target-keys) (list "key" key-fields))))

(define (sniff-for-pii row rule)
  (let ([row-results  (vector-map rule row)])
    (foldl (lambda (column-result result)
             (if (cadr column-result)
                 (list (add1 (car result)) (car column-result) )
                 (list (car result) (car column-result) )))
           '(0 "")
           (vector->list row-results))))

(module+ test
  (define row-result #(1 "user@example.com"))
  
  (test-case "sniff-for-pii run rules over each row looking for PII"
    (define rule-mock (mock #:behavior (const '("email adddress" #f))))
    (sniff-for-pii row-result rule-mock)
    (check-mock-called-with? rule-mock (arguments (vector-ref row-result 0)))
    (check-mock-called-with? rule-mock (arguments (vector-ref row-result 1))))
  (test-case "sniff-for-pii returns a count of the PII instances detected"
    (define rule-mock (mock #:behavior (const '("email address" #t))))
    (check-equal? (car (sniff-for-pii row-result rule-mock)) 2))
  (test-case "sniff-for-pii returns the name of the rule when PII is detected"
    (define rule-mock (mock #:behavior (const '("email address" #t))))
    (check-equal? (cadr (sniff-for-pii row-result rule-mock)) "email address"))
  (test-case "sniff-for-pii returns an count of 0 when no PII is detected"
    (define rule-mock (mock #:behavior (const '("email address" #f))))
    (check-equal? (cadr (sniff-for-pii row-result rule-mock)) "email address")))





