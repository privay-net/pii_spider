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

(define (create-data-table rows)
  (txexpr 'table empty
          (txexpr 'tr empty
                 (txexpr 'th empty (list "Key")))))

(module+ test
  (test-case "create-data-table will create a table presenting the result"
    (define result "<table><<tr>th>Key</th><th>Rule</th></tr></table>")
    (define test-row (list 1 2 3 4))
    (check-equal? (xexpr->string (create-data-table test-row)) result)))
