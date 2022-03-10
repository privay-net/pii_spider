#lang racket/base
(require racket/function)

(provide log-thread)

(define-logger agent)
(define agent-logger-receiver (make-log-receiver agent-logger 'info 'agent))
(current-logger agent-logger)

(define (write-to-file destination content)
  (with-output-to-file destination
    (λ ()
      (display 
       content))
    #:mode 'text #:exists 'append))

(define (write-to-port destination content)
  (display content destination))

(define log-destinations
  (list (list write-to-file "pii_agent.log")
        (list write-to-port (current-error-port))))

(define (send-log-content content destinations)
  (unless (null? destinations)
    (let ([ destination (car destinations)])
      ((car destination) (cadr destination) content)) 
    (send-log-content content (cdr destinations))))

; Listen for events on the log-receiver
(define log-thread
  (thread 
   (thunk
    (let loop ()
      (define log-vector (sync agent-logger-receiver))
      (define content (format "[~a] ~a\n"
                              (vector-ref log-vector 0)
                              (vector-ref log-vector 1)))
      (send-log-content content log-destinations)
      (when (thread-try-receive)
        (kill-thread (current-thread)))
      (loop)))))
