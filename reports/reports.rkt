#lang racket
(require txexpr)
(require "../examined-rows.rkt")

(module+ test
  (require rackunit)
  (require mock)
  (require mock/rackunit))

(provide html-report)

;; TODO deal with empty results
;; TODO deal with multiple tables
(define (html-report rows #:table-creator [row-table row-table])
  (define wrapper (list 'html '(head) (txexpr 'body empty (row-table rows))))
  (xexpr->html wrapper))

(module+ test
  (test-case "html-report produces a HTML report of the run"
    (define result "<html><head></head><body>mock table</body></html>")
    (define test-rows '())
    (define table-creator-mock (mock #:behavior (const  (list  "mock table"))))
    (check-equal? (html-report test-rows #:table-creator table-creator-mock) result))
  (test-case "html-report calls the table-creator"
    (define test-rows '())
    (define table-creator-mock (mock #:behavior (const  (list  "mock table"))))
    (html-report test-rows #:table-creator table-creator-mock)
    (check-mock-called-with?  table-creator-mock (arguments test-rows))))

(define (row-table rows #:row-creator [create-data-table-rows create-data-table-rows])
  (txexpr* 'table empty
           (txexpr* 'tr empty
                    (txexpr 'th empty (list "Key"))
                    (txexpr 'th empty (list "Rule")))
           (create-data-table-rows rows)))

(module+ test
  (test-case "row-table will create a table presenting the result"
    (define result "<table><tr><th>Key</th><th>Rule</th></tr><tr><td>No rows were examined</td></tr></table>")
    (define test-row empty)
    (check-equal? (xexpr->html (row-table test-row)) result))
  (test-case "row-table calls the row-creator"
    (define test-rows '())
    (define mock-result (txexpr 'tr empty (list "test")))
    (define row-creator-mock (mock #:behavior (const mock-result)))
    (row-table test-rows #:row-creator row-creator-mock)
    (check-mock-called-with?  row-creator-mock (arguments test-rows))))

(define (create-data-table-rows rows)
  (if (empty? rows)
      (txexpr* 'tr empty
               (txexpr 'td empty '("No rows were examined")))
      (txexpr* 'tr empty
               (txexpr 'td empty (hash-ref (examined-row-id rows) "key"))
               (txexpr* 'td empty
                        (rule-list (examined-row-results rows))))))

(module+ test
  (test-case "create-data-table-rows will return a defult message if the row list is empty"
    (define result "<tr><td>No rows were examined</td></tr>")
    (define no-rows empty)
    (check-equal? (xexpr->html (create-data-table-rows no-rows)) result))
  (test-case "create-data-table-rows will create a data row"
    (define result "<tr><td>Blah</td><td>Blah</td></tr>")
    (define test-row (examined-row (hash "key" '(1)) '((1 "email address") (1 "AU phone number"))))
    (check-equal? (xexpr->html (create-data-table-rows test-row)) result)))

;; TODO can I do this using recursion?
(define (rule-list rules)
  (if (empty? rules)
      (txexpr 'p empty '("No rules to display."))
      (txexpr* 'ul empty
               (for ([rule rules])
                 (txexpr 'li empty (list (cadr rule)))))))

(module+ test
  (test-case "rule-list will return a default message if the rule list is empty"
    (define result "<p>No rules to display.</p>")
    (define no-rules empty)
    (check-equal? (xexpr->html (rule-list no-rules)) result))
  (test-case "rule-list will return an unordered list of each rule"
    (define result "<ul>No rules to display.</ul>")
    (define two-rules '((1 "email address") (1 "AU phone number")))
    (check-equal? (xexpr->html (rule-list two-rules)) result)))
