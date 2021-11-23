#lang racket
(require txexpr)
(require gregor)
(require "../structs.rkt")

(module+ test
  (require rackunit)
  (require mock)
  (require mock/rackunit))

(provide html-table-report)

(define (html-table-report table-results #:table-creator [row-table row-table] #:summary-creator [results-summary results-summary])
  (define report (txexpr* 'html '((lang "en") (class "no-js"))
                           (txexpr* 'head empty
                                    (txexpr 'meta '((charset "UTF-8")))
                                    (txexpr 'meta '((name "viewport")
                                                    (content "width=device-width, initial-scale=1")))
                                    (txexpr 'title empty '("PII Spider Report"))
                                    (txexpr 'meta '((name "description")
                                                    (content "Report on PII discovered in this database"))))
                           (txexpr* 'body empty
                                    (results-summary table-results)
                                    (row-table (examined-table-results table-results)))))
  (string-append "<!DOCTYPE html>" (xexpr->html report)))

(module+ test
  (define test-no-rows '())
  (define test-two-rows (list  (examined-row (hash "key" '(1))
                                             '((1 "email address") (1 "AU phone number")))
                               (examined-row (hash "key" '(2))
                                             '((1 "email address") (1 "AU phone number"))))) 
  (define start-time (moment 1970))
  (define end-time (moment 2000 02 28 13 14 30))
  (define test-two-record-table (examined-table "two_rows" start-time end-time 2 test-two-rows))
  (define test-zero-record-table (examined-table "no_rows" start-time end-time 0 test-no-rows))
  
  (test-case "html-table-report produces a HTML report of the run"
    (define result "<!DOCTYPE html><html lang=\"en\" class=\"no-js\"><head><meta charset=\"UTF-8\"/><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"/><title>PII Spider Report</title><meta name=\"description\" content=\"Report on PII discovered in this database\"/></head><body><p>mock summary</p><p>mock table</p></body></html>")
    (define table-creator-mock (mock #:behavior (const (txexpr 'p empty (list "mock table")))))
    (define summary-mock (mock #:behavior (const (txexpr 'p empty (list "mock summary")))))
    (check-equal? (html-table-report test-zero-record-table #:table-creator table-creator-mock #:summary-creator summary-mock) result))
  (test-case "html-table-report calls the table-creator"
    (define table-creator-mock (mock #:behavior (const (txexpr 'p empty (list "mock table")))))
    (html-table-report test-zero-record-table #:table-creator table-creator-mock)
    (check-mock-called-with?  table-creator-mock (arguments (examined-table-results test-zero-record-table))))
  (test-case "html-table-report produces a report for the table"
    (define result "<!DOCTYPE html><html lang=\"en\" class=\"no-js\"><head><meta charset=\"UTF-8\"/><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"/><title>PII Spider Report</title><meta name=\"description\" content=\"Report on PII discovered in this database\"/></head><body><div><h1>Results for table two_rows</h1><table><tr><td>Start Time:</td><td>1970-01-01 00:00:00 +1000</td></tr><tr><td>End Time:</td><td>2000-02-28 13:14:00 +1100</td></tr><tr><td>Rows Examined:</td><td>2</td></tr></table></div><table><caption>Results for table</caption><thead><tr><th>Key</th><th>Rule</th></tr></thead><tbody><tr><td>1</td><td><ul class=\"rule-list\"><li>email address</li><li>AU phone number</li></ul></td></tr><tr><td>2</td><td><ul class=\"rule-list\"><li>email address</li><li>AU phone number</li></ul></td></tr></tbody></table></body></html>")
    (check-equal? (html-table-report test-two-record-table) result)))

(define (results-summary results)
  (txexpr* 'div empty
           (txexpr 'h1 empty (list (string-append "Results for table " (examined-table-name results))))
           (txexpr* 'table empty
                    (txexpr* 'tr empty
                             (txexpr 'td empty '("Start Time:"))
                             (txexpr 'td empty (list (~t (examined-table-start-time results) "yyyy-MM-dd HH:mm:SS Z"))))
                    (txexpr* 'tr empty
                             (txexpr 'td empty '("End Time:"))
                             (txexpr 'td empty (list (~t (examined-table-end-time results) "yyyy-MM-dd HH:mm:SS Z"))))
                    (txexpr* 'tr empty
                             (txexpr 'td empty '("Rows Examined:"))
                             (txexpr 'td empty (list (number->string (examined-table-row-count results))))))))

(module+ test
  (test-case "results-summary will create a div summarising the results of the table"
    (define result "<div><h1>Results for table no_rows</h1><table><tr><td>Start Time:</td><td>1970-01-01 00:00:00 +1000</td></tr><tr><td>End Time:</td><td>2000-02-28 13:14:00 +1100</td></tr><tr><td>Rows Examined:</td><td>0</td></tr></table></div>")
    (check-equal? (xexpr->html (results-summary test-zero-record-table)) result)))

(define (row-table rows #:row-creator [row-table-body row-table-body])
  (txexpr* 'table empty
           (txexpr 'caption empty '("Results for table"))
           (row-table-header)
           (row-table-body rows)))

(module+ test
  (test-case "row-table will create a table presenting an empty result"
    (define result "<table><caption>Results for table</caption><thead><tr><th>Key</th><th>Rule</th></tr></thead><tbody><tr><td>No rows were examined</td></tr></tbody></table>")
    (define test-row empty)
    (check-equal? (xexpr->html (row-table test-row)) result))
  (test-case "row-table will create a table presenting the result"
    (define result "<table><caption>Results for table</caption><thead><tr><th>Key</th><th>Rule</th></tr></thead><tbody><tr><td>1</td><td><ul class=\"rule-list\"><li>email address</li><li>AU phone number</li></ul></td></tr><tr><td>2</td><td><ul class=\"rule-list\"><li>email address</li><li>AU phone number</li></ul></td></tr></tbody></table>")
    (define test-rows (list  (examined-row (hash "key" '(1))
                                           '((1 "email address") (1 "AU phone number")))
                             (examined-row (hash "key" '(2))
                                           '((1 "email address") (1 "AU phone number")))))
    (check-equal? (xexpr->html (row-table test-rows)) result))
  (test-case "row-table calls the row-creator"
    (define test-rows '())
    (define mock-result (txexpr 'tr empty (list "test")))
    (define row-creator-mock (mock #:behavior (const mock-result)))
    (row-table test-rows #:row-creator row-creator-mock)
    (check-mock-called-with?  row-creator-mock (arguments test-rows))))

(define (row-table-header)
  (txexpr* 'thead
           empty
           (txexpr* 'tr empty
                   (txexpr 'th empty (list "Key"))
                   (txexpr 'th empty (list "Rule")))))

(module+ test
  (test-case "row-table will create a table presenting the result"
    (define result "<thead><tr><th>Key</th><th>Rule</th></tr></thead>")
    (check-equal? (xexpr->html (row-table-header)) result)))

(define (row-table-body rows)
  (if (empty? rows)

      (txexpr* 'tbody empty
               (txexpr* 'tr empty
                        (txexpr 'td empty '("No rows were examined"))))
      (cons 'tbody (map (lambda (row)
                          (txexpr* 'tr empty
                                   (txexpr 'td empty
                                           (list (key->string
                                                  (hash-ref (examined-row-id row) "key"))))
                                   (txexpr* 'td empty
                                            (rule-list (examined-row-results row))))) rows))))

(module+ test
  (test-case "row-table-body will return a defult message if the row list is empty"
    (define result '(tbody (tr (td "No rows were examined"))))
    (define no-rows empty)
    (check-equal? (row-table-body no-rows) result))
  (test-case "row-table-body will create a data row with a single examined-row"
    (define result '(tbody (tr (td "1")
                               (td (ul ((class "rule-list"))
                                       (li "email address")
                                       (li "AU phone number"))))))
    (define test-row (list (examined-row (hash "key" '(1))
                                         '((1 "email address") (1 "AU phone number")))))
    (check-equal? (row-table-body test-row) result))
  (test-case "row-table-body will create a data row with a complex key"
    (define result '(tbody (tr (td "1, 2, three")
                               (td (ul ((class "rule-list"))
                                       (li "email address")
                                       (li "AU phone number"))))))
    (define test-row (list (examined-row (hash "key" '(1 2 "three"))
                                         '((1 "email address") (1 "AU phone number")))))
    (check-equal? (row-table-body test-row) result))
  (test-case "row-table-body will create multiple data rows with multiple examined-rows"
    (define result  '(tbody
                      (tr (td "1") (td (ul ((class "rule-list"))
                                           (li "email address")
                                           (li "AU phone number"))))
                      (tr (td "2") (td (ul ((class "rule-list"))
                                           (li "email address")
                                           (li "AU phone number"))))))
    (define test-rows (list  (examined-row (hash "key" '(1))
                                           '((1 "email address") (1 "AU phone number")))
                             (examined-row (hash "key" '(2))
                                           '((1 "email address") (1 "AU phone number")))))
    (check-equal? (row-table-body test-rows) result)))

(define (key->string key)
  (string-join (map (lambda (part-key) (format "~a" part-key)) key) ", "))

(module+ test
  (test-case "key->string returns a single number as a string"
    (define test-key '(1))
    (define result "1")
    (check-equal? (key->string test-key) result))
  (test-case "key->string returns a mixture of numbers as a string"
    (define test-key '(1 2.0))
    (define result "1, 2.0")
    (check-equal? (key->string test-key) result))
  (test-case "key->string returns a mixture of numbers and strings as a string"
    (define test-key '(1 "a" 2.1))
    (define result "1, a, 2.1")
    (check-equal? (key->string test-key) result)))

(define (rule-list rules)
  (if (empty? rules)
      (txexpr 'p empty '("No rules to display."))
      (txexpr 'ul '((class "rule-list"))
              (for/list ([rule rules])
                (quasiquote (li (unquote (cadr rule))))))))

(module+ test
  (test-case "rule-list will return a default message if the rule list is empty"
    (define result "<p>No rules to display.</p>")
    (define no-rules empty)
    (check-equal? (xexpr->html (rule-list no-rules)) result))
  (test-case "rule-list will return an unordered list of each rule"
    (define result "<ul class=\"rule-list\"><li>email address</li><li>AU phone number</li></ul>")
    (define two-rules '((1 "email address") (1 "AU phone number")))
    (check-equal? (xexpr->html (rule-list two-rules)) result)))
