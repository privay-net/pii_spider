#lang racket/base

(require rackunit
         pii_spider/pii/rules)

(provide rules-tests)

(define rules-tests
  (test-suite
   "rules"
   (test-suite
    "email"
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
   (test-suite
    "au-phone-number"
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
   (test-suite
    "credit-card"
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
   (test-suite
    "au-tax-file-number"
    (test-case "returns the rule name"
      (check-equal? (car (au-tax-file-number "test")) "AU Tax File Number"))
    (test-case "returns #f when not a AU tax file number"
      (check-false (cadr (au-tax-file-number "test"))))
    (test-case "returns #t for a valid AU TFN"
      (check-true (cadr (au-tax-file-number "123456782"))))
    (test-case "returns #t for a valid AU TFN with spaces"
      (check-true (cadr (au-tax-file-number "123 456 782")))))
   (test-suite
    "au-tax-file-number"
    (test-case "returns #t for a valid AU TFN"
      (check-true (cadr (au-tax-file-number "123456782"))))
    (test-case "returns #f for an invalid AU TFN"
      (check-false (cadr (au-tax-file-number "123456789")))))
   (test-suite
    "password"
    (test-case "returns the rule name"
      (check-equal? (car (password "test")) "password"))
    (test-case "returns #f when not a likely password"
      (check-false (cadr (password "test"))))
    (test-case "returns #t for a likely password preceeded by the word password"
      (check-true (cadr (password "password: passw0rd")))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests rules-tests))
