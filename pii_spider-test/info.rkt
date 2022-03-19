#lang info

(define collection "pii_spider")
(define deps '("base"
               "txexpr"
               "gregor"))
(define build-deps '("rackunit-lib"
                     "mock"
                     "cover"
                     "cover-coveralls"
                     "mock-rackunit"
                     "pii_spider-lib"))

(define update-implies '("pii_spider-lib"))
  
(define test-omit-paths '("dev" "coverage"))
(define clean '("compiled" "tests/compiled" "tests/private/compiled"))
