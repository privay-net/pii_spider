#lang racket

(provide email)

;; TODO have this maybe with levels of expense for deeper checking i.e. level 1 - regexp level 2 - domain check level 3 - test email
(define (email candidate)
  (define simple-email-regex (pregexp "^\\S+@\\S+\\.\\S+$"))
  (list "email address" (regexp-match? simple-email-regex candidate)))

(module+ test
  (require rackunit
           mock
           mock/rackunit)

  (test-case "returns the rule name"
    (check-equal? (car (email "test")) "email address"))
  (test-case "returns #t for an email address"
    (check-true (cadr (email "robert@test.com"))))
  (test-case "returns #f when not an email address"
    (check-false (cadr (email "test")))))

