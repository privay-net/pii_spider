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

;; Check this handy helper for more CC number formats
;; https://en.wikipedia.org/wiki/Payment_card_number
;; https://stackoverflow.com/questions/9315647/regex-credit-card-number-tests
;; https://www.creditcardinsider.com/learn/anatomy-of-a-credit-card/
;; https://www.paypalobjects.com/en_GB/vhelp/paypalmanager_help/credit_card_numbers.htm
;; TODO handle number fields correctly
;; TODO handle more card issuers
;; TODO validate the card numbers more thoroughly
(define (credit-card candidate)
  (define visa-mc-regex (pregexp "[452]\\d{3}[\\s-]?\\d{4}[\\s-]?\\d{4}[\\s-]?\\d{4}"))
  (define amex-regex (pregexp "3[47]\\d{2}[\\s-]?\\d{6}[\\s-]?\\d{5}"))
  (if (string? candidate)
      (list "Credit Card" (or (regexp-match? visa-mc-regex candidate)
                              (regexp-match? amex-regex candidate)))
      (list "Credit Card" #f)))

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

(define (au-tax-file-number candidate)
  (if (string? candidate)
      (list "AU Tax File Number" (validate-tfn candidate))
      (list "AU Tax File Number" #f)))

(module+ test
  (test-case "returns the rule name"
    (check-equal? (car (au-tax-file-number "test")) "AU Tax File Number"))
  (test-case "returns #f when not a AU tax file number"
    (check-false (cadr (au-tax-file-number "test"))))
  (test-case "returns #t for a valid AU TFN"
    (check-true (cadr (au-tax-file-number "123456782"))))
  (test-case "returns #t for a valid AU TFN with spaces"
    (check-true (cadr (au-tax-file-number "123 456 782")))))

;; see https://www.clearwater.com.au/code/tfn for the procedure used to calculate this
(define (validate-tfn candidate)
  (define tfn-regex (pregexp "(\\d{3})\\s?(\\d{3})\\s?(\\d{3})"))
  (define magic-weights '(1 4 3 7 5 8 6 9 10))
  (if (regexp-match tfn-regex candidate)
      (zero?
       (remainder 
        (foldr + 0  (map * magic-weights
                         (map string->number
                              (map string
                                   (string->list
                                    (foldr string-append ""
                                           (cdr (regexp-match tfn-regex candidate)))))))) 11))
      #f))

(module+ test
  (test-case "returns #t for a valid AU TFN"
    (check-true (validate-tfn "123456782")))
  (test-case "returns #f for an invalid AU TFN"
    (check-false (validate-tfn "123456789"))))
