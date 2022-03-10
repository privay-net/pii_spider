#lang info

(define collection "pii_spider")
(define deps '("base"))
(define build-deps '("scribble-lib"
                     "racket-doc"))
(define scribblings '(("scribblings/pii_spider.scrbl" (multi-page))))
(define clean '("compiled" "doc" "doc/pii_spider"))
