#lang racket/base

(require racket/function
         rackunit
         mock
         mock/rackunit
         gregor
         pii_spider/crawlers/postgresql
         pii_spider/structs)


(provide postgresql-tests)

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
  
(define postgresql-tests
  (test-suite
   "postgresql"
   (test-suite
    "crawl-postgresql"
     (test-case "crawl-postgresql sends the db details to initialise-connection"
      (crawl-postgresql test-settings #:connector connector-mock
                        #:list-tables list-tables-mock
                        #:table-examiner examine-tables-mock
                        #:summary-reporter save-html-summary-report-mock
                        #:table-reporter save-report-mock
                        #:summary-updater update-html-summary-report-mock)
      (check-mock-called-with? connector-mock (arguments test-settings)))
     (test-case "crawl-postgresql raises an exception when initialise-connection fails"
       (define connector-mock (mock #:behavior (const void)))
       (define crawl-attempt (lambda () 
                               (crawl-postgresql test-settings #:connector connector-mock
                                                 #:list-tables list-tables-mock
                                                 #:table-examiner examine-tables-mock
                                                 #:summary-reporter save-html-summary-report-mock
                                                 #:table-reporter save-report-mock
                                                 #:summary-updater update-html-summary-report-mock)))
       (check-exn exn:fail:pii-spider:db-connection? crawl-attempt))
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
      (check-mock-called-with? examine-tables-mock (arguments test-connection empty-ignore "test"))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests postgresql-tests))
