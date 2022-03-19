#lang racket/base

(require racket/cmdline)
(require pii_spider)

(module+ main
  (define logging-thread log-thread)
  (define settings (add-environment-vars (default-settings)))
  
  (command-line
   #:program "pii_spider"
   #:once-each
   [("-d" "--database") database "the database URL to connect to"
                        (hash-set! settings 'database database)]
   [("-u" "--username") username "the username to connect with"
                        (hash-set! settings 'username username)]
   [("-p" "--password") password "the password to connect with"
                        (hash-set! settings 'password password)]
   [("-s" "--server") server "the server to connect with"
                      (hash-set! settings 'server server)]
   [("-P" "--port") port "the port to connect to"
                    (hash-set! settings 'port (string->number port))]
   [("-I" "--ignorefile") ignore-file "the location of a JSON file specifying what to ignore"
                          (hash-set! settings 'ignoreFile ignore-file)]
   #:args ()
   (crawler settings))
  
  (thread-send logging-thread 'time-to-stop)
  (thread-wait logging-thread))

