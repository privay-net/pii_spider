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
  (define wrapper (list 'html '(head) (txexpr* 'body empty (row-table rows))))
  (xexpr->html wrapper))

(module+ test
  (test-case "html-report produces a HTML report of the run"
    (define result "<html><head></head><body><p>mock table</p></body></html>")
    (define test-rows '())
    (define table-creator-mock (mock #:behavior (const (txexpr 'p empty (list "mock table")))))
    (check-equal? (html-report test-rows #:table-creator table-creator-mock) result))
  (test-case "html-report calls the table-creator"
    (define test-rows '())
    (define table-creator-mock (mock #:behavior (const (txexpr 'p empty (list "mock table")))))
    (html-report test-rows #:table-creator table-creator-mock)
    (check-mock-called-with?  table-creator-mock (arguments test-rows)))
  (test-case "html-report produces a report for the table"
    (define result "<html><head></head><body><table><tr><th>Key</th><th>Rule</th></tr><tr><td>&#1;</td><td><ul class=\"rule-list\"><li>email address</li><li>AU phone number</li></ul></td></tr><tr><td>&#2;</td><td><ul class=\"rule-list\"><li>email address</li><li>AU phone number</li></ul></td></tr></table></body></html>")
    (define test-rows (list  (examined-row (hash "key" '(1))
                                           '((1 "email address") (1 "AU phone number")))
                             (examined-row (hash "key" '(2))
                                           '((1 "email address") (1 "AU phone number"))))) 
    (check-equal? (html-report test-rows) result)))

(define (row-table rows #:row-creator [create-data-table-rows create-data-table-rows])
  (txexpr 'table empty (cons (table-header-row) (create-data-table-rows rows))))

(module+ test
  (test-case "row-table will create a table presenting an empty result"
    (define result "<table><tr><th>Key</th><th>Rule</th></tr><tr><td>No rows were examined</td></tr></table>")
    (define test-row empty)
    (check-equal? (xexpr->html (row-table test-row)) result))
   (test-case "row-table will create a table presenting the result"
     (define result "<table><tr><th>Key</th><th>Rule</th></tr><tr><td>&#1;</td><td><ul class=\"rule-list\"><li>email address</li><li>AU phone number</li></ul></td></tr><tr><td>&#2;</td><td><ul class=\"rule-list\"><li>email address</li><li>AU phone number</li></ul></td></tr></table>")
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

(define (table-header-row)
  (txexpr* 'tr empty
           (txexpr 'th empty (list "Key"))
           (txexpr 'th empty (list "Rule"))))

(module+ test
  (test-case "row-table will create a table presenting the result"
    (define result "<tr><th>Key</th><th>Rule</th></tr>")
    (check-equal? (xexpr->html (table-header-row)) result)))

(define (create-data-table-rows rows)
  (if (empty? rows)
      ;; the reason I'm puttiing a list here is to make the interface regular
      (list (txexpr* 'tr empty
                     (txexpr 'td empty '("No rows were examined"))))
      (map (lambda (row)
             (txexpr* 'tr empty
                      (txexpr 'td empty (hash-ref (examined-row-id row) "key"))
                      (txexpr* 'td empty
                               (rule-list (examined-row-results row))))) rows)))

(module+ test
  (test-case "create-data-table-rows will return a defult message if the row list is empty"
    (define result '((tr (td "No rows were examined"))))
    (define no-rows empty)
    (check-equal? (create-data-table-rows no-rows) result))
  (test-case "create-data-table-rows will create a data row with a single examined-row"
    (define result '((tr (td 1)
                         (td (ul ((class "rule-list"))
                                 (li "email address")
                                 (li "AU phone number"))))))
    (define test-row (list (examined-row (hash "key" '(1))
                                         '((1 "email address") (1 "AU phone number")))))
    (check-equal? (create-data-table-rows test-row) result))
   (test-case "create-data-table-rows will create multiple data rows with multiple examined-rows"
     (define result  '((tr (td 1) (td (ul ((class "rule-list"))
                                          (li "email address")
                                          (li "AU phone number"))))
                       (tr (td 2) (td (ul ((class "rule-list"))
                                          (li "email address")
                                          (li "AU phone number"))))))
    (define test-rows (list  (examined-row (hash "key" '(1))
                                           '((1 "email address") (1 "AU phone number")))
                             (examined-row (hash "key" '(2))
                                           '((1 "email address") (1 "AU phone number")))))
    (check-equal? (create-data-table-rows test-rows) result)))

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
