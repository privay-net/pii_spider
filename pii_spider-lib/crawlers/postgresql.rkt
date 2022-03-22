#lang racket/base

(require racket/vector)
(require racket/exn)
(require db)
(require gregor)
(require "../structs.rkt")
(require (prefix-in rules: "../pii/rules.rkt"))
(require "../reports/reports.rkt")
(require "../ignore.rkt")
(require "../exceptions.rkt")

(module+ test
  (require rackunit)
  (require mock)
  (require mock/rackunit)
  (require racket/function))

(provide crawl-postgresql)

(define (crawl-postgresql settings #:connector [initialise-connection initialise-connection]
                 #:list-tables [list-tables list-tables]
                 #:table-examiner [examine-table examine-table]
                 #:ignore-directives [generate-ignore-lists generate-ignore-lists]
                 #:summary-reporter [save-html-summary-report save-html-summary-report]
                 #:table-reporter [save-report save-report]
                 #:summary-updater [update-html-summary-report update-html-summary-report])
  
  ; connect to the db
  (log-info "Connecting to the database")
  (define pgc (initialise-connection settings))

  ;  work out what to ignore
  (log-info "Reading ignore file")
  (define ignores (generate-ignore-lists settings))
  
  ; find all the tables
  (log-info "Examining database structure")
  (define tables (list-tables pgc))

  ;; initialise summary report
  (log-info "Initialise the output report")
  (define report-location (save-html-summary-report #:output-dir (hash-ref settings 'outputDir)))
  
  ;; grab rows of data from each table, return pii rows
  ;;  TODO Note everything after listing tables should be parallel-ised to make it faster
  (map (lambda (table)
         (log-info (format "Examining table ~a" table))
         ;; deal with taking some small number of rows vs scanning the entire thing
         (define results (examine-table pgc ignores table))
         (save-report results #:output-dir (hash-ref settings 'outputDir))
         (update-html-summary-report table (string-append table ".html") ignores))
       tables)

  ; return summary report location
  (log-info (format "Run report at ~a" (path->string report-location))))

(module+ test
  (define empty-ignore (ignore null (hasheq) (hasheq)))
  (define ignore-mock (mock #:behavior (const empty-ignore)))
  (define-opaque test-connection)
  (define connector-mock (mock #:behavior (const test-connection)))
  (define test-list-tables '("test"))
  (define list-tables-mock (mock #:behavior (const test-list-tables)))
  (define test-two-rows (list  (examined-row (hash "key" '(1))
                                             '((1 "email address") (1 "AU phone number")))
                               (examined-row (hash "key" '(2))
                                             '((1 "email address") (1 "AU phone number"))))) 
  (define start-time (moment 1970))
  (define end-time (moment 2000 02 28 13 14 30))
  (define test-examined-table (examined-table "two_rows" start-time end-time 2 test-two-rows #f))
  

  (define examine-tables-mock (mock #:behavior (const test-examined-table)))
  (define test-settings (hash 'username "robert" 'password "bhujasample4$"
                              'database "pii" 'server "localhost"
                              'port 5432 'ignoreFile "ignore.json"
                              'outputDir (build-path (current-directory) "test")))

  (define save-html-summary-report-mock (mock
                                         #:behavior (const (build-path (current-directory) "test"))))
  (define-opaque test-file)
  (define save-report-mock (mock
                     #:behavior (const test-file)))
  (define update-html-summary-report-mock (mock
                     #:behavior (const test-file)))
  
  (test-case "crawl-postgresql sends the db details to initialise-connection"
    (crawl-postgresql test-settings #:connector connector-mock
             #:list-tables list-tables-mock
             #:table-examiner examine-tables-mock
             #:summary-reporter save-html-summary-report-mock
             #:table-reporter save-report-mock
             #:summary-updater update-html-summary-report-mock)
    (check-mock-called-with? connector-mock (arguments test-settings)))
  (test-case "crawl-postgresql compiles a list of tables to examine"
    (crawl-postgresql test-settings #:connector connector-mock
             #:list-tables list-tables-mock
             #:table-examiner examine-tables-mock
             #:summary-reporter save-html-summary-report-mock
             #:table-reporter save-report-mock
             #:summary-updater update-html-summary-report-mock)
    (check-mock-called-with? list-tables-mock (arguments test-connection)))
  (test-case "crawl-postgresql examines each table"
    (crawl-postgresql test-settings #:connector connector-mock
             #:list-tables list-tables-mock
             #:table-examiner examine-tables-mock
             #:ignore-directives ignore-mock
             #:summary-reporter save-html-summary-report-mock
             #:table-reporter save-report-mock
             #:summary-updater update-html-summary-report-mock)
    (check-mock-called-with? examine-tables-mock (arguments test-connection empty-ignore "test"))))

(define (initialise-connection credentials #:connector [postgresql-connect postgresql-connect])
  (with-handlers ([exn:fail:network:errno? (lambda (e)
                                             (log-info "Could not connect to the database server")
                                             (log-debug (exn->string e)))]
                  [exn:fail:sql? (lambda (e)
                                  (log-info "Could not connect to the database with your credentials")
                                  (log-debug (exn->string e)))])
    (postgresql-connect #:user (hash-ref credentials 'username)
                        #:database (hash-ref credentials 'database)
                        #:password (hash-ref credentials 'password)
                        #:server (hash-ref credentials 'server)
                        #:port (hash-ref credentials 'port))))


(module+ test
  ;;; TODO pool connections
  (test-case "initialise-connection gets the connection for a database"
    (initialise-connection test-settings #:connector connector-mock)
    (check-mock-called-with? connector-mock
                             (arguments #:database (hash-ref test-settings 'database)
                                        #:password (hash-ref test-settings 'password)
                                        #:user (hash-ref test-settings 'username)
                                        #:server (hash-ref test-settings 'server)
                                        #:port (hash-ref test-settings 'port))))
  (test-case "initialise-connection throws an exception when it can't connect"
    ;; error 61 is the I can't open that tcp port apparently
    (define connector-mock (mock #:behavior (thunk* (raise (exn:fail:network:errno "test" (current-continuation-marks) '(61 . posix))))))
    (check-not-exn (lambda () (initialise-connection test-settings #:connector connector-mock))))
  (test-case "initialise-connection throws an exception when the auth or db details are incorrect"
    (define connector-mock (mock #:behavior (thunk* (raise (exn:fail:sql "test-sql" (current-continuation-marks) "test 2" "test 3" )))))
    ;; (define connector-mock (mock #:behavior (thunk* (postgresql-connect #:database (hash-ref test-settings 'database)
    ;;                                                                     #:password "wrong"
    ;;                                                                     #:user (hash-ref test-settings 'username)
    ;;                                                                     #:server (hash-ref test-settings 'server)
    ;;                                                                     #:port (hash-ref test-settings 'port)))))
    (check-not-exn (lambda () (initialise-connection test-settings #:connector connector-mock)))))

(define (examine-table connection ignores table-name
                       #:row-function [retrieve-rows retrieve-rows]
                       #:row-estimate [estimate-row-count estimate-row-count]
                       #:row-examiner [examine-rows examine-rows])
  ;; TODO detect a difference between the count I get from the DB when starting versus the count
  ;; I get later
  (define start-time (now/moment))
  (define row-count (estimate-row-count connection table-name))
  (define rules (list rules:email rules:au-phone-number rules:credit-card rules:au-tax-file-number))
  ;; only examine the rows if the table is NOT ignored
  (define rows (if (member table-name (ignore-tables ignores))
                   null
                   (retrieve-rows connection table-name)))
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
    (examine-table connector-mock empty-ignore "foo" #:row-function row-mock
                   #:row-estimate row-count-mock #:row-examiner row-examiner-mock)
    (check-mock-called-with? row-mock (arguments connector-mock "foo")))

  (test-case "examine-table checks the row count with the required arguments"
    (examine-table connector-mock empty-ignore "foo" #:row-function row-mock
                   #:row-estimate row-count-mock #:row-examiner row-examiner-mock)
    (check-mock-called-with? row-count-mock (arguments connector-mock "foo")))
  
  (test-case "examine-table exectutes a row examiner with the required arguments"
    (examine-table connector-mock empty-ignore "foo" #:row-function row-mock
                   #:row-estimate row-count-mock #:row-examiner row-examiner-mock)
    (check-mock-called-with? row-mock (arguments connector-mock "foo")))

  (test-case "examine-table gives the row examiner no rows if the table should be ignored"
    (mock-reset! row-examiner-mock)
    (examine-table connector-mock (ignore '("foo") (hasheq) (hasheq)) "foo" #:row-function row-mock
                   #:row-estimate row-count-mock #:row-examiner row-examiner-mock)
    (check-mock-calls row-examiner-mock (list (arguments null (list rules:email rules:au-phone-number rules:credit-card rules:au-tax-file-number)))))
  
  (test-case "examine-table returns an examined-table struct with appropriate values"
    (define result (examine-table connector-mock empty-ignore "foo" #:row-function row-mock #:row-estimate row-count-mock #:row-examiner row-examiner-mock))
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
                             #:start-time [start-time (now/moment)]
                             #:row-count [row-count 0]
                             #:results [results null]
                             #:ignored [ignored #f])
  (examined-table table-name start-time (now/moment) row-count results ignored))

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
    (check-equal? (examined-table-results (initialise-metadata "test")) null))
  (test-case "initialise-metadata returns with the results set if overriden"
    (check-equal? (examined-table-results
                   (initialise-metadata "test" #:results '(examined-rows)))
                  '(examined-rows)))
  (test-case "initialise-metadata returns with the start-time set to now by default"
    ; this check only works becuase of computers are so fast. I could mock now but that seems excessive
    (check-equal? (seconds-between (examined-table-start-time (initialise-metadata "test")) (now/moment)) 0)) 
  (test-case "initialise-metadata returns with the start-time set if overriden"
    (define test-start-time (moment 2000))
    (check-equal? (examined-table-start-time (initialise-metadata "test" #:start-time test-start-time)) test-start-time))
  (test-case "initialise-metadata returns with the table ignored set to false by default"
    (check-false (examined-table-ignored (initialise-metadata "test"))))
  (test-case "initialise-metadata returns with the table ignored set if overriden"
    (check-true (examined-table-ignored (initialise-metadata "test" #:ignored #t)))))

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



