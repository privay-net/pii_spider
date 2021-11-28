#lang racket

(require db)
(require gregor)
(require "structs.rkt")
(require (prefix-in rules: "pii/rules.rkt"))
(require "reports/reports.rkt")

(provide crawler)
(define (crawler url #:connector [initialise-connection initialise-connection]
                 #:list-tables [list-tables list-tables])
  ; connect to the db
  (define pgc (initialise-connection))

  ; find all the tables
  (define tables (list-tables pgc))
  ; deal with taking some small number of rows vs scanning the entire thing
  ; grab rows of data from each table
  ;; (define rows (examine-table pgc "users"))
  ; have a set of rules applied to each set of table rows
  (define rules (list rules:email rules:au-phone-number))
  ; look in each row for pii data
  ; return pii rows
  ;; (examine-rows rows rules)
  ;; (save-report) 
  ;;  TODO Note everything after listing tables should be parallel-ised to make it faster
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
  (test-case "crawler returns a report structure"
    (check-eq?
     (crawler "not://a.url/test"
              #:connector connector-mock
              #:list-tables list-tables-mock) "not://a.url/test"))
  
  (test-case "crawler compiles a list of tables to examine"
    (crawler "not://a.url/test" #:connector connector-mock #:list-tables list-tables-mock)
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

(define (examine-table connection table-name
                       #:row-function [retrieve-rows retrieve-rows]
                       #:row-estimate [estimate-row-count estimate-row-count]
                       #:row-examiner [examine-rows examine-rows])
  ;; TODO detect a difference between the count I get from the DB when starting versus the count
  ;; I get later
  (define start-time (now))
  (define row-count (estimate-row-count connection table-name))
  (define rows (retrieve-rows connection table-name))
  (define rules (list rules:email rules:au-phone-number))
  (define results (examine-rows rows rules))
  (initialise-metadata table-name #:start-time start-time #:row-count row-count #:results results))

(module+ test
  (define row-count-mock (mock #:behavior (const 1)))
  (define one-row-result '(#(1 "user@example.com")))
  (define row-mock (mock #:behavior (const one-row-result)))
  (define examined-row-result (list  (examined-row (hash "key" '(1))
                                                   '((1 "email address") (1 "AU phone number")))))
  (define row-examiner-mock (mock #:behavior (const examined-row-result)))
  
  ;;; TODO deal with table not existing error  
  (test-case "examine-table exectutes a row query with the required arguments"
    (examine-table connector-mock "foo" #:row-function row-mock
                   #:row-estimate row-count-mock #:row-examiner row-examiner-mock)
    (check-mock-called-with? row-mock (arguments connector-mock "foo")))

  (test-case "examine-table checks the row count with the required arguments"
    (examine-table connector-mock "foo" #:row-function row-mock
                   #:row-estimate row-count-mock #:row-examiner row-examiner-mock)
    (check-mock-called-with? row-count-mock (arguments connector-mock "foo")))
  
  (test-case "examine-table exectutes a row examiner with the required arguments"
    (examine-table connector-mock "foo" #:row-function row-mock
                   #:row-estimate row-count-mock #:row-examiner row-examiner-mock)
    (check-mock-called-with? row-mock (arguments connector-mock "foo")))
  
  (test-case "examine-table returns an examined-table struct with appropriate values"
    (define result (examine-table connector-mock "foo" #:row-function row-mock #:row-estimate row-count-mock #:row-examiner row-examiner-mock))
    (check-equal? (examined-table-results result) examined-row-result)
    (check-equal? (examined-table-row-count result) 1)))

(define (estimate-row-count connection table-name #:query-function [query-value query-value])
    (let ([query (string-append "select count(*) from \"" table-name "\";")])
      (query-value connection query)))

(module+ test
  ;;; TODO deal with table not existing error  
  (test-case "estimate-row-count exectutes a query with the required arguments"
    (define one-row-result 1)
    (define-opaque connection)
    (define query-mock (mock #:behavior (const one-row-result)))
    (estimate-row-count connection "foo" #:query-function query-mock)
    (check-mock-called-with? query-mock (arguments connection "select count(*) from \"foo\";")))
  
  (test-case "estimate-row-count returns a count of the rows"
    (define one-row-result 1)
    (define query-mock (mock #:behavior (const one-row-result)))
    (check-eq? (estimate-row-count connector-mock "foo" #:query-function query-mock) one-row-result)))

(define (retrieve-rows connection table-name #:query-function [query-rows query-rows])
  (let ([query (string-append "select * from \"" table-name "\";")])
    (query-rows connection query)))

(module+ test
  (define query-mock (mock #:behavior (const one-row-result)))
  
  (test-case "retreive-rows executes a query with the correct arguments"
    (retrieve-rows connector-mock "bar" #:query-function query-mock)
    (check-mock-called-with? query-mock (arguments connector-mock "select * from \"bar\";")))
  (test-case "retrieve-rows returns a list of rows to examine"
    (define one-row-result '(#(1 "user@example.com")))
    (check-equal? (retrieve-rows connector-mock "baz" #:query-function query-mock) one-row-result)))

(define (initialise-metadata table-name
                             #:start-time [start-time (now)]
                             #:row-count [row-count 0]
                             #:results [results empty])
  (examined-table table-name start-time (now) row-count results))

(module+ test
  (test-case "initialise-metadata returns an examined-table"
    (check-true (examined-table? (initialise-metadata "test"))))
  (test-case "initialise-metadata returns with the table name set"
    (check-equal? (examined-table-name (initialise-metadata "test")) "test"))
  (test-case "initialise-metadata returns with the table row-count set to 0 by default"
    (check-equal? (examined-table-row-count (initialise-metadata "test")) 0))
  (test-case "initialise-metadata returns with the table row-count set if overriden"
    (check-equal? (examined-table-row-count (initialise-metadata "test" #:row-count 1)) 1))
  (test-case "initialise-metadata returns with the results set to empty by default"
    (check-equal? (examined-table-results (initialise-metadata "test")) empty))
  (test-case "initialise-metadata returns with the results set if overriden"
    (check-equal? (examined-table-results
                   (initialise-metadata "test" #:results '(examined-rows)))
                  '(examined-rows)))
  (test-case "initialise-metadata returns with the start-time set to now by default"
    ; this check only works becuase of computers are so fast. I could mock now but that seems excessive
    (check-equal? (seconds-between (examined-table-start-time (initialise-metadata "test")) (now)) 0)) 
  (test-case "initialise-metadata returns with the start-time set if overriden"
    (define test-start-time (moment 2000))
    (check-equal? (examined-table-start-time (initialise-metadata "test" #:start-time test-start-time)) test-start-time)))

(define (examine-rows rows rules #:examiner-function [crawl-for-pii crawl-for-pii])
  (map (lambda (row)
         (examined-row (extract-primary-key row)
                       (map (lambda (rule)
                              (crawl-for-pii row rule))
                            rules))) rows))

(module+ test
  (test-case "examine-rows applies crawl-for-pii to each row and rule"
    (define crawl-for-pii-mock (mock #:behavior (const (void)) ))
    (define rows '(#(1 "robert@test.com" "0412345678" "Robert")
                   #(2 "rob@test.com" "0412345679" "Rob")))
    (define rule1-mock (mock #:behavior (const (void)) ))
    (define rule2-mock (mock #:behavior (const (void)) ))
    (define rules (list rule1-mock rule2-mock))
    (examine-rows rows rules #:examiner-function crawl-for-pii-mock)
    (check-mock-called-with? crawl-for-pii-mock (arguments (car rows) (car rules)))
    (check-mock-called-with? crawl-for-pii-mock (arguments (cadr rows) (car rules)))
    (check-mock-called-with? crawl-for-pii-mock (arguments (car rows) (cadr rules)))
    (check-mock-called-with? crawl-for-pii-mock (arguments (cadr rows) (cadr rules)))))

(define (extract-primary-key row [primary-key-locations '(0)])
  (hash "key" 
        (map  (lambda (location) (vector-ref row location)) primary-key-locations)))

(module+ test
  (test-case "extract-primary-key returns the value of the first column as the default primary key"
    (define primary-key 1)
    (define row (vector primary-key "robert@test.com" "0412345678" "Robert"))
    (check-equal? (extract-primary-key row) (hash "key" (list primary-key))))
  (test-case "extract-primary-key returns a list of values as the primary key"
    (define target-keys '(0 1 2))
    (define key-fields '(1 "robert@test.com" "0412345678"))
    (define row (list->vector (append key-fields '("Robert"))))
    (check-equal? (extract-primary-key row target-keys) (hash "key" key-fields))))

(define (crawl-for-pii row rule)
  (let ([row-results  (vector-map rule row)])
    (foldl (lambda (column-result result)
             (if (cadr column-result)
                 (list (add1 (car result)) (car column-result) )
                 (list (car result) (car column-result) )))
           '(0 "")
           (vector->list row-results))))

(module+ test
  (define row-result #(1 "user@example.com"))
  
  (test-case "crawl-for-pii run rules over each row looking for PII"
    (define rule-mock (mock #:behavior (const '("email adddress" #f))))
    (crawl-for-pii row-result rule-mock)
    (check-mock-called-with? rule-mock (arguments (vector-ref row-result 0)))
    (check-mock-called-with? rule-mock (arguments (vector-ref row-result 1))))
  (test-case "crawl-for-pii returns a count of the PII instances detected"
    (define rule-mock (mock #:behavior (const '("email address" #t))))
    (check-equal? (car (crawl-for-pii row-result rule-mock)) 2))
  (test-case "crawl-for-pii returns the name of the rule when PII is detected"
    (define rule-mock (mock #:behavior (const '("email address" #t))))
    (check-equal? (cadr (crawl-for-pii row-result rule-mock)) "email address"))
  (test-case "crawl-for-pii returns an count of 0 when no PII is detected"
    (define rule-mock (mock #:behavior (const '("email address" #f))))
    (check-equal? (cadr (crawl-for-pii row-result rule-mock)) "email address")))

(define pgc (initialise-connection))
(define rows (examine-table pgc "users"))
(define rules (list rules:email rules:au-phone-number))
rows
;; TODO fix the struct generation so source-rows go in source rows and reults go in result rows
;; (define report (html-table-report results) )



