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
(define (html-report rows #:table-creator [create-data-table create-data-table])
  (define wrapper (list 'html '(head) (txexpr 'body empty (create-data-table rows))))
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

(define (create-data-table rows #:row-creator [create-data-table-rows create-data-table-rows])
  (txexpr* 'table empty
           (txexpr* 'tr empty
                    (txexpr 'th empty (list "Key"))
                    (txexpr 'th empty (list "Rule")))
           (create-data-table-rows rows)))

(module+ test
  (test-case "create-data-table will create a table presenting the result"
    (define result "<table><tr><th>Key</th><th>Rule</th></tr></table>")
    (define test-row (list 1 2 3 4))
    (check-equal? (xexpr->html (create-data-table test-row)) result))
  (test-case "create-data-table calls the row-creator"
    (define test-rows '(1 2))
    (define mock-result (txexpr 'tr empty (list "test")))
    (define row-creator-mock (mock #:behavior (const mock-result)))
    (create-data-table test-rows #:row-creator row-creator-mock)
    (check-mock-called-with?  row-creator-mock (arguments test-rows))))

(define (create-data-table-rows rows)
  (txexpr* 'tr empty
           (txexpr 'td empty (list "blah"))
           (txexpr 'td empty (list "blah"))))

(module+ test
  (test-case "create-data-table-rows will create a set of rows"
    (define result "<tr><td>Blah</td><td>Blah</td></tr>")
    (define test-rows '(1 2))
    (check-equal? (xexpr->html (create-data-table-rows test-rows)) result)))
