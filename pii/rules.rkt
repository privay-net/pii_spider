#lang racket

(provide email au-phone-number)

;; TODO have this maybe with levels of expense for deeper checking i.e. level 1 - regexp level 2 - domain check level 3 - test email
(define (email candidate)
  (define simple-email-regex (pregexp "\\S+@\\S+\\.\\S+"))
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
  (test-case "returns #t for an email address inside a string"
    (check-true (cadr (email "the string is about robert@test.com being tested"))))
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
(test-case "returns #t for an AU phone number inside a larger string"
    (check-true (cadr (au-phone-number "we should ring 0412 345 678")))
    (check-true (cadr (au-phone-number "0412 345678 is a nice phone number")))) 
  (test-case "returns #f when not an AU phone number"
    (check-false (cadr (au-phone-number "test"))))
  (test-case "returns #f when not a string"
    (check-false (cadr (au-phone-number 1)))))

(define (credit-card candidate)
  (define visa-mc-regex (pregexp "[452]\\d{3}[\\s-]?\\d{4}[\\s-]?\\d{4}[\\s-]?\\d{4}"))
  (define amex-regex (pregexp "3[47]\\d{2}[\\s-]?\\d{6}[\\s-]?\\d{5}"))
  (list "Credit Card" (or (regexp-match? visa-mc-regex candidate)
                          (regexp-match? amex-regex candidate))))

(module+ test
  (test-case "returns the rule name"
    (check-equal? (car (credit-card "test")) "Credit Card"))
  (test-case "returns #t for a valid visa card number"
    (check-true (cadr (credit-card "4111111111111111"))))
  (test-case "returns #t for a valid visa card number with spaces"
    (check-true (cadr (credit-card "4111 1111 1111 1111"))))
  (test-case "returns #t for a valid visa card number with spaces inside a string"
    (check-true (cadr (credit-card "a credit card called 4111 1111 1111 1111 is hidden here"))))
  (test-case "returns #t for a valid visa card number with hyphens"
    (check-true (cadr (credit-card "4111-1111-1111-1111"))))
  (test-case "returns #t for a valid visa card number with spaces and hypens"
    (check-true (cadr (credit-card "4111 1111-1111 1111"))))
  (test-case "returns #t for a valid amex card number"
    (check-true (cadr (credit-card "371238839571772"))))
  (test-case "returns #t for a valid amex card number with spaces"
    (check-true (cadr (credit-card "3712 388395 71772"))))
  (test-case "returns #f when not a credit card number"
    (check-false (cadr (credit-card "test")))))
