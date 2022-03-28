#lang racket/base

(require racket/cmdline)
(require racket/exn)
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
   [("-s" "--server") server "the server to connect with defaults to localhost"
                      (hash-set! settings 'server server)]
   [("-P" "--port") port "the port to connect to defaults to 5432"
                    (hash-set! settings 'port (string->number port))]
   [("-D" "--daemon")  "the app should be started as a daemon"
                       (hash-set! settings 'daemon #t)]
   [("-I" "--ignorefile") ignore-file
                          "the location of a JSON file specifying what to ignore, defaults to ignore.json"
                          (hash-set! settings 'ignoreFile ignore-file)]
   [("-O" "--outputDir") output-dir
                         "the directory to output to, defaults to current directory/output" (hash-set! settings 'outputDir output-dir)]
   #:args () (void))

  (with-handlers ([exn:fail? (lambda (e)
                               (log-error "Runtime Error:")
                               (log-error (exn->string e))
                               (exit 1))])
    (crawl-postgresql settings))  
  
  (thread-send logging-thread 'time-to-stop)
  (thread-wait logging-thread))

