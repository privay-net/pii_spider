#lang racket/base
(require racket/vector)
(require "crawl.rkt")

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

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

