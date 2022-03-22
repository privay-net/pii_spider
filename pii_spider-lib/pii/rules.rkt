#lang racket/base

(require racket/match)
(require racket/string)

(provide email au-phone-number credit-card au-tax-file-number password)

;; TODO have this maybe with levels of expense for deeper checking i.e. level 1 - regexp level 2 - domain check level 3 - test email
(define (email candidate)
  (define simple-email-regex (pregexp "\\S+@\\S+\\.\\S+"))
  (if (string? candidate)
      (list "email address" (regexp-match? simple-email-regex candidate))
      (list "email address" #f)))

(define (au-phone-number candidate)
  (define simple-regex #px"[+61|0]\\d{3}\\s?\\d{3}\\s?\\d{3}")
  (if (string? candidate)
      (list "AU phone number" (regexp-match? simple-regex candidate))
      (list "AU phone number" #f)))

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

(define (au-tax-file-number candidate)
  (if (string? candidate)
      (list "AU Tax File Number" (validate-tfn candidate))
      (list "AU Tax File Number" #f)))

;; see https://www.clearwater.com.au/code/tfn for the procedure used to calculate this
(define (validate-tfn candidate)
  (define tfn-regex (pregexp "(\\d{3})\\s?(\\d{3})\\s?(\\d{3})"))
  (define magic-weights '(1 4 3 7 5 8 6 9 10))
  (define 0-code (char->integer #\0))
  (match candidate
    [(pregexp tfn-regex (cons _ (app string-append* s)))
     (zero? (remainder
             (for/sum ([c (in-string s)]
                       [w (in-list magic-weights)])
               (* (- (char->integer c) 0-code) w))
             11))]
    [_ #f]))

;; TODO have a go at medicare

(define (password candidate)
  (define simple-regex #px"password[:]?[\\s]+")
  (if (string? candidate)
      (list "password" (regexp-match? simple-regex candidate))
      (list "password" #f)))

