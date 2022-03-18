#lang info
(define collection 'multi)
(define deps '("base"
               "pii_spider-lib"
               "pii_spider-doc"
               "pii_spider-test"))
(define implies '("pii_spider-lib"
                  "pii_spider-doc"
                  "pii_spider-test"))
(define version "0.1")
(define license '(MIT))
