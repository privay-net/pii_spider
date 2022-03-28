#lang racket/base

(require web-server/dispatch
         web-server/servlet-env
         net/url-string
         gregor
         koyo/json)

(provide listen dispatcher 500-responder 404-responder examine)

(define (listen #:engine [serve/servlet serve/servlet])
  (serve/servlet
   dispatcher
   #:servlet-responder 500-responder
   #:port 8080
   #:listen-ip #f
   #:launch-browser? #f
   #:servlet-regexp #rx""
   #:file-not-found-responder 404-responder
   #:log-file "requests.log"))

(define-values (dispatcher dispatch-url)
  (dispatch-rules
   [("examine" (string-arg)) #:method "post" examine]))

(define (500-responder url ex)
  (log-error (format  "[ ~a]  ~a  --->  ~a"
                      (datetime->iso8601 (now/utc))
                      (url->string url)
                      (exn-message ex)))
  (response/json
   #hash((hasError . #t))
   #:code 500))

(define (404-responder req)
  (response/json null #:code 404))

(define (examine request)
  (response/json
   #hash((test . #t))
   #:code 200))

