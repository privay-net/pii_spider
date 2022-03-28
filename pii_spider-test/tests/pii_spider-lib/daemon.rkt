#lang racket/base

(require racket/function
         racket/port
         json
         pii_spider/daemon
         rackunit
         mock
         mock/rackunit
         web-server/http/response-structs)

(provide daemon-tests)

(define daemon-tests
  (test-suite
   "daemon"
   (test-suite
    "listen"
    ;; TODO add some integration tests to check the 404/500 handling
    (test-case "spins up the servlet engine with the correct args"
      (define servlet-mock (mock #:behavior (const (void))))
      (listen #:engine servlet-mock)
      (check-mock-called-with? servlet-mock (arguments dispatcher
                                                       #:servlet-regexp #rx""
                                                       #:launch-browser? #f
                                                       #:servlet-responder 500-responder
                                                       #:file-not-found-responder 404-responder
                                                       #:listen-ip #f
                                                       #:port 8080
                                                       #:log-file "requests.log"))))
   (test-suite
    "examine"
    (test-case "examine returns a 200 for a successful response"
      (define mock-request (mock #:behavior (const (void))))
      (define result (examine mock-request))
      (check-equal? (response-code result) 200))
    (test-case "examine returns JSON for a successful response"
      (define mock-request (mock #:behavior (const (void))))
      (define result (examine mock-request))
      (check-equal? (call-with-output-bytes (response-output result))
                    (jsexpr->bytes #hash((test . #t))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests daemon-tests))
