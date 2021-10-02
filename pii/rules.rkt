#lang racket

(provide email au-phone-number)

;; TODO have this maybe with levels of expense for deeper checking i.e. level 1 - regexp level 2 - domain check level 3 - test email
(define (email candidate)
  (define simple-email-regex (pregexp "^\\S+@\\S+\\.\\S+$"))
  (if (string? candidate)
      (list "email address" (regexp-match? simple-email-regex candidate))
      (list "email address" #f)))

(module+ test
  (require rackunit
           mock
           mock/rackunit)
  (test-case "returns the rule name"
    (check-equal? (car (email "test")) "email address"))
  (test-case "returns #t for an email address"
    (check-true (cadr (email "robert@test.com"))))
  (test-case "returns #f when not an email address"
    (check-false (cadr (email "test"))))
  (test-case "returns #f when not a string"
    (check-false (cadr (email 1)))))

(define (au-phone-number candidate)
  (define simple-regex #px"[+61|0]\\d{3}\\s?\\d{3}\\s?\\d{3}")
  (if (string? candidate)
      (list "AU phone number" (regexp-match? simple-regex candidate))
      (list "AU phone number" #f)))

(module+ test
  (test-case "returns the rule name"
    (check-equal? (car (au-phone-number "test")) "AU phone number"))
  (test-case "returns #t for an AU phone number"
    (check-true (cadr (au-phone-number "0412345678"))))
  (test-case "returns #t for an AU phone number with country prefix"
   (check-true (cadr (au-phone-number "+61412345678"))))
  (test-case "returns #t for an AU phone number with spaces"
    (check-true (cadr (au-phone-number "0412 345 678")))
    (check-true (cadr (au-phone-number "0412 345678"))))
  (test-case "returns #f when not an AU phone number"
    (check-false (cadr (au-phone-number "test"))))
  (test-case "returns #f when not a string"
    (check-false (cadr (au-phone-number 1)))))
