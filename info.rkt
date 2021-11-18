#lang info
(define collection "pii-spider")
(define deps '("base" "gregor" "txexpr"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "mocks" "rackunit-mocks"))
(define scribblings '(("scribblings/pii_spider.scrbl" ())))
(define pkg-desc "Find PII data in your database")
(define version "0.0")
(define pkg-authors '(robertpostill))
