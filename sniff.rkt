#lang racket

(require db)

(provide sniffer)
(define (sniffer url #:connector [initialise-connection initialise-connection]
                     #:list-tables [list-tables list-tables])
  ; connect to the db
  (define pgc (initialise-connection))

  ; find all the tables
  (list-tables pgc)
  ; deal with taking some small number of rows vs scanning the entire thing
  ; check for pii_data in each table
  ; return report
  url
  )

(define (initialise-connection #:connector [postgresql-connect postgresql-connect])
      (postgresql-connect #:user "robert"
                        #:database "pii"
                        #:password "bhujasample4$"))
(define (examine-table table-name connection)
  #t)

(module+ test
  (require rackunit)
  (require mock)
  (require mock/rackunit)

  (define-opaque test-connection)
  (define-opaque test-list-tables)
  
  (test-case "sniffer returns a report structure"
   (check-eq? (sniffer "not://a.url/test") "not://a.url/test"))

  (test-case "sniffer compiles a list of tables to examine"
    (define connector-mock (mock #:behavior (const test-connection)))
    (define list-tables-mock (mock #:behavior (const test-list-tables)))
    
    (sniffer "not://a.url/test" #:connector connector-mock #:list-tables list-tables-mock)
    (check-mock-called-with? list-tables-mock (arguments test-connection)))

  (test-case "initialise-connection gets the connection for a database"
    (define connector-mock (mock #:behavior (const test-connection)))
    (initialise-connection #:connector connector-mock)
    (check-mock-called-with? connector-mock (arguments #:database "pii"
                                                       #:password "bhujasample4$"
                                                       #:user "robert")))

  
  (test-case "examine-table gets the rows from the table"
    (check-eq? (examine-table "users" test-connection) #t)))
