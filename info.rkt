#lang info
(define collection "pii-sniffer")
(define deps '("base" "gregor" "txexpr"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "mocks" "rackunit-mocks"))
(define scribblings '(("scribblings/pii_sniffer.scrbl" ())))
(define pkg-desc "Sniff out PII data")
(define version "0.0")
(define pkg-authors '(robertpostill))
