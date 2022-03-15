#lang info

(define collection "pii_spider")
(define deps '("base"))
(define build-deps '("rackunit-lib"
                     "mock"
                     "cover"
                     "cover-coveralls"
                     "mock-rackunit"
                     "pii_spider-lib"
                     "relation"))
(define test-include-paths '("tests"))
(define compile-omit-paths '("tests"))
(define test-omit-paths '("dev" "coverage"))
(define clean '("compiled" "tests/compiled" "tests/private/compiled"))
