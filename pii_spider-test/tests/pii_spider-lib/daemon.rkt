#lang racket/base

(require racket/function
         racket/port
         racket/logging
         json
         net/url
         pii_spider/daemon
         rackunit
         mock
         mock/rackunit
         web-server/http/response-structs
         pii_spider/structs)

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
                    (jsexpr->bytes #hash((test . #t))))))
   (test-suite
    "404-responder"
    (test-case "404-responder returns a 404"
      (define mock-request (mock #:behavior (const (void))))
      (define result (404-responder mock-request))
      (check-equal? (response-code result) 404))
    (test-case "404-responder returns JSON"
      (define mock-request (mock #:behavior (const (void))))
      (define result (404-responder mock-request))
      (check-equal? (call-with-output-bytes (response-output result))
                    (jsexpr->bytes null))))
(test-suite
    "500-responder"
    (test-case "500-responder returns a status code of 500"
      (define test-url (string->url "http://localhost"))
      (define ex (exn:fail:pii-spider "test error" (current-continuation-marks)))
      (define test-logger (make-logger #f (current-logger) 'none #f))
      (define result (let ([my-log (open-output-nowhere)])
                       (with-logging-to-port
                         my-log
                         (lambda ()
                           (current-logger test-logger)
                           (500-responder test-url ex))
                         'error
                         #:logger test-logger)))
      (check-equal? (response-code result) 500))
    (test-case "500-responder returns JSON for a successful response"
      (define test-url (string->url "http://localhost"))
      (define ex (exn:fail:pii-spider "test error" (current-continuation-marks)))
      (define test-logger (make-logger #f (current-logger) 'none #f))
      (define result (let ([my-log (open-output-nowhere)])
                       (with-logging-to-port
                         my-log
                         (lambda ()
                           (current-logger test-logger)
                           (500-responder test-url ex))
                         'error
                         #:logger test-logger)))
      (check-equal? (call-with-output-bytes (response-output result))
                    (jsexpr->bytes #hash((hasError . #t))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests daemon-tests))
