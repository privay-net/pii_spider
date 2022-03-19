
#lang racket/base

(require json)
(require racket/port)
(require "structs.rkt")
(module+ test
  (require racket/function)
  (require rackunit)
  (require mock)
  (require mock/rackunit))

(provide generate-ignore-lists)

(define (generate-ignore-lists settings
                               #:file-check [file-exists? file-exists?]
                               #:ignore-directives [extract-ignore-directives extract-ignore-directives])
  (define ignore-file (hash-ref settings 'ignoreFile))
  (log-info "Checking for ignore file")
  (if (file-exists? ignore-file)
      (extract-ignore-directives ignore-file)
      (ignore null null null)))

(module+ test
  (define test-settings (hash 'ignoreFile "ignore.json"))
  
  (test-case "generate-ignore-lists returns an empty ignore struct if there's no ignore file"
    (define mock-file-test (mock #:behavior (const #f)))
    (define result (generate-ignore-lists test-settings #:file-check mock-file-test))
    (check-equal? (ignore-tables result) null)
    (check-equal? (ignore-columns result) null)
    (check-equal? (ignore-rows result) null))
  (test-case "generate-ignore-lists uses extract-ignore-directives if the ignore-file exists"
    (define ignore-directives-mock (mock #:behavior (const null)))
    (define mock-file-test (mock #:behavior (const #t)))
    (generate-ignore-lists test-settings #:ignore-directives ignore-directives-mock #:file-check mock-file-test)
    (check-mock-called-with? ignore-directives-mock (arguments "ignore.json"))))

(define (extract-ignore-directives ignore-file #:file-input [with-input-from-file with-input-from-file])
  (define ignore-nothing (lambda (exn) (hasheq 'ignoreTables null 'ignoreColumns (hasheq) 'ignoreRows (hasheq))))
  (define ignore-hash (with-handlers ([exn:fail:filesystem? ignore-nothing]
                                      [exn:fail:read? ignore-nothing])
                        (with-input-from-file ignore-file
                          (lambda () (read-json)))))
  (ignore (hash-ref ignore-hash 'ignoreTables null)
          (hash-ref ignore-hash 'ignoreColumns (hasheq))
          (hash-ref ignore-hash 'ignoreRows (hasheq))))

(module+ test
  (test-case "extract-ignore-directives returns an empty ignore list if the file can't be read"
    (define mock-file-read (mock #:behavior (thunk* (raise (exn:fail:filesystem:exists "test" (current-continuation-marks))))))
    (define result (extract-ignore-directives test-settings #:file-input mock-file-read))
    (check-true (ignore? result))
    (check-true (null? (ignore-tables result)))
    (check-true (hash-empty? (ignore-columns result)))
    (check-true (hash-empty? (ignore-rows result))))
  (test-case "extract-ignore-directives returns an empty ignore list if it's not JSON"
    (define mock-file-read (mock #:behavior (thunk* (with-input-from-string "Not JSON" (lambda () (read-json))))))
    (define result (extract-ignore-directives test-settings #:file-input mock-file-read))
    (check-true (ignore? result))
    (check-true (null? (ignore-tables result)))
    (check-true (hash-empty? (ignore-columns result)))
    (check-true (hash-empty? (ignore-rows result))))
  (test-case "extract-ignore-directives returns a list of tables to ignore if given tables to ignore"
    (define mock-file-read (mock #:behavior (const (hasheq 'ignoreTables '("ignore1")))))
    (check-equal? (ignore-tables (extract-ignore-directives test-settings #:file-input mock-file-read))
                  '("ignore1")))
  (test-case "extract-ignore-directives returns a hash of columns to ignore if given columns to ignore"
    (define mock-file-read (mock #:behavior (const (hasheq 'ignoreColumns (hasheq 'table1 '("column1"))))))
    (check-equal? (ignore-columns (extract-ignore-directives test-settings #:file-input mock-file-read))
                  (hasheq 'table1 '("column1"))))
  (test-case "extract-ignore-directives returns a list of rows to ignore if given rows to ignore"
    (define mock-file-read (mock #:behavior (const (hasheq 'ignoreRows (hasheq 'table2 '(1))))))
    (check-equal? (ignore-rows (extract-ignore-directives test-settings #:file-input mock-file-read))
                  (hasheq 'table2 '(1))))
  (test-case "extract-ignore-directives returns an empty list of tables to ignore if there's no tables to ignore"
    (define mock-file-read (mock #:behavior (const (hasheq 'doesntMatter '("ignore1")))))
    (check-equal? (ignore-tables (extract-ignore-directives test-settings #:file-input mock-file-read))
                  null))
  (test-case "extract-ignore-directives returns an empty hash of columns to ignore if there's no columns to ignore"
    (define mock-file-read (mock #:behavior (const (hasheq 'doesntMatter '("ignore1")))))
    (check-true (hash-empty?
                 (ignore-columns (extract-ignore-directives test-settings #:file-input mock-file-read)))))
  (test-case "extract-ignore-directives returns an empty hash of rows to ignore if there's no rows to ignore"
    (define mock-file-read (mock #:behavior (const (hasheq 'doesntMatter '("ignore1")))))
    (check-true (hash-empty?
                 (ignore-rows (extract-ignore-directives test-settings #:file-input mock-file-read))))))


