#lang racket/base
(require "crawl.rkt")

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline)

  (define-logger agent)
  (define agent-logger-receiver (make-log-receiver agent-logger 'info 'agent))
  (current-logger agent-logger)

  (define (write-to-file destination content)
    (with-output-to-file destination
      (λ ()
        (displayln 
         content))
      #:mode 'text #:exists 'append))

  (define (write-to-port destination content)
    (displayln content destination))

  (define log-destinations
    (list (list write-to-file "pii_agent.log")
          (list write-to-port (current-error-port))))

  (define (send-log-content content destinations)
    (unless (null? destinations)
      (let ([ destination (car destinations)])
        ((car destination) (cadr destination) content)) 
      (send-log-content content (cdr destinations))))

  ; Listen for events on the log-receiver
  (void 
   (thread 
    (λ()(let loop ()
          (define log-vector (sync agent-logger-receiver))
          (define content (format "[~a] ~a\n"
                                  (vector-ref log-vector 0)
                                  (vector-ref log-vector 1)))
          
          (send-log-content content log-destinations)
          (loop)))))
  
  (define settings (make-hash))
  (hash-set! settings 'server "localhost")
  (hash-set! settings 'port 5432)
  (hash-set! settings 'ignoreFile "ignore.json")
  
  (command-line
   #:program "pii_spider"
   #:once-each
   [("-d" "--database") database "the database URL to connect to" (hash-set! settings 'database database)]
   [("-u" "--username") username "the username to connect with" (hash-set! settings 'username username)]
   [("-p" "--password") password "the password to connect with" (hash-set! settings 'password password)]
   [("-s" "--server") server "the server to connect with" (hash-set! settings 'server server)]
   [("-P" "--port") port "the port to connect to" (hash-set! settings 'port (string->number port))]
   [("-I" "--ignorefile") ignore-file "the location of a JSON file specifying what to ignore" (hash-set! settings 'ignore-file ignore-file)]
   #:args ()
   (crawler settings)))

