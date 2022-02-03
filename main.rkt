#lang racket/base
(require racket/vector)
(require "crawl.rkt")

(module+ test
  (require rackunit))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline)
  
  (define credentials (make-hash))
  (hash-set! credentials 'server "localhost")
  (hash-set! credentials 'port 5432)
  
  (command-line
   #:program "pii_spider"
   #:once-each
   [("-d" "--database") database "the database URL to connect to" (hash-set! credentials 'database database)]
   [("-u" "--username") username "the username to connect with" (hash-set! credentials 'username username)]
   [("-p" "--password") password "the password to connect with" (hash-set! credentials 'password password)]
   [("-s" "--server") server "the server to connect with" (hash-set! credentials 'server server)]
   [("-P" "--port") port "the port to connect to" (hash-set! credentials 'port (string->number port))]
   #:args ()
   (crawler credentials)))

