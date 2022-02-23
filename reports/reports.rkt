#lang racket
(require txexpr)
(require gregor)
(require "../structs.rkt")

(module+ test
  (require rackunit)
  (require mock)
  (require mock/rackunit))

(provide html-table-report save-report save-html-summary-report update-html-summary-report)

(define (html-table-report table-results #:table-creator [row-table row-table] #:summary-creator [results-summary results-summary])
  (define report (txexpr* 'html '((lang "en") (class "no-js"))
                          (txexpr* 'head empty
                                   (txexpr 'meta '((charset "UTF-8")))
                                   (txexpr 'meta '((name "viewport")
                                                   (content "width=device-width, initial-scale=1")))
                                   (txexpr 'title empty '("PII Spider Report"))
                                   (txexpr 'meta '((name "description")
                                                   (content "Report on PII discovered in this database")))
                                   (txexpr 'link '((href "https://unpkg.com/tailwindcss@^1.0/dist/tailwind.min.css") (rel "stylesheet"))))
                          (txexpr* 'body empty
                                   (txexpr* 'div '((class "min-w-screen min-h-screen bg-gray-200 flex-col p-3 overflow-auto"))
                                            (txexpr 'h1 '((class "text-center text-5xl font-extrabold")) (list (string-append "Report for table " (examined-table-name table-results))))
                                            (results-summary table-results)
                                            (row-table (examined-table-results table-results))))))
  (string-append "<!DOCTYPE html>" (xexpr->html report)))

(module+ test
  (define test-no-rows '())
  (define test-two-rows (list  (examined-row (hash "key" '(1))
                                             '((1 "email address") (1 "AU phone number")))
                               (examined-row (hash "key" '(2))
                                             '((1 "email address") (1 "AU phone number"))))) 
  (define start-time (moment 1970))
  (define end-time (moment 2000 02 28 13 14 30))
  (define test-two-record-table (examined-table "two_rows" start-time end-time 2 test-two-rows))
  (define test-zero-record-table (examined-table "no_rows" start-time end-time 0 test-no-rows))
  
  (test-case "html-table-report produces a HTML report of the run"
    (define result "<!DOCTYPE html><html lang=\"en\" class=\"no-js\"><head><meta charset=\"UTF-8\"/><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"/><title>PII Spider Report</title><meta name=\"description\" content=\"Report on PII discovered in this database\"/><link href=\"https://unpkg.com/tailwindcss@^1.0/dist/tailwind.min.css\" rel=\"stylesheet\"/></head><body><div class=\"min-w-screen min-h-screen bg-gray-200 flex-col p-3 overflow-auto\"><h1 class=\"text-center text-5xl font-extrabold\">Report for table no_rows</h1><p>mock summary</p><p>mock table</p></div></body></html>")
    (define table-creator-mock (mock #:behavior (const (txexpr 'p empty (list "mock table")))))
    (define summary-mock (mock #:behavior (const (txexpr 'p empty (list "mock summary")))))
    (check-equal? (html-table-report test-zero-record-table #:table-creator table-creator-mock #:summary-creator summary-mock) result))
  (test-case "html-table-report calls the table-creator"
    (define table-creator-mock (mock #:behavior (const (txexpr 'p empty (list "mock table")))))
    (html-table-report test-zero-record-table #:table-creator table-creator-mock)
    (check-mock-called-with?  table-creator-mock (arguments (examined-table-results test-zero-record-table))))
  (test-case "html-table-report produces a report for the table"
    (define result "<!DOCTYPE html><html lang=\"en\" class=\"no-js\"><head><meta charset=\"UTF-8\"/><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"/><title>PII Spider Report</title><meta name=\"description\" content=\"Report on PII discovered in this database\"/><link href=\"https://unpkg.com/tailwindcss@^1.0/dist/tailwind.min.css\" rel=\"stylesheet\"/></head><body><div class=\"min-w-screen min-h-screen bg-gray-200 flex-col p-3 overflow-auto\"><h1 class=\"text-center text-5xl font-extrabold\">Report for table two_rows</h1><div class=\"p-3 m-2 bg-white rounded-md\"><h2 class=\"text-3xl\">Statistics</h2><table class=\"table-auto w-full\"><tr><td class=\"text-right pr-4\">Start Time:</td><td>1970-01-01 00:00:00</td></tr><tr><td class=\"text-right pr-4\">End Time:</td><td>2000-02-28 13:14:00</td></tr><tr><td class=\"text-right pr-4\">Rows Examined:</td><td>2</td></tr></table></div><div class=\"p-3 m-2 bg-white rounded-md\"><h2 class=\"text-3xl\">Results</h2><table class=\"table-auto w-full\"><thead><tr><th>Key</th><th>Rule</th></tr></thead><tbody><tr class=\"border-b-2 border-gray-300\"><td class=\"text-center\">1</td><td><ul><li class=\"float-left p-1 m-4 border-2 rounded-full\"><span class=\"p1\">email address</span></li><li class=\"float-left p-1 m-4 border-2 rounded-full\"><span class=\"p1\">AU phone number</span></li></ul></td></tr><tr class=\"border-b-2 border-gray-300\"><td class=\"text-center\">2</td><td><ul><li class=\"float-left p-1 m-4 border-2 rounded-full\"><span class=\"p1\">email address</span></li><li class=\"float-left p-1 m-4 border-2 rounded-full\"><span class=\"p1\">AU phone number</span></li></ul></td></tr></tbody></table></div></div></body></html>")
    (check-equal? (html-table-report test-two-record-table) result)))

(define (results-summary results)
  (txexpr* 'div '((class "p-3 m-2 bg-white rounded-md"))
           (txexpr 'h2 '((class "text-3xl")) (list "Statistics"))
           (txexpr* 'table '((class "table-auto w-full"))
                    (txexpr* 'tr empty
                             (txexpr 'td '((class "text-right pr-4")) '("Start Time:"))
                             (txexpr 'td empty (list (~t (examined-table-start-time results) "yyyy-MM-dd HH:mm:SS"))))
                    (txexpr* 'tr empty
                             (txexpr 'td '((class "text-right pr-4")) '("End Time:"))
                             (txexpr 'td empty (list (~t (examined-table-end-time results) "yyyy-MM-dd HH:mm:SS"))))
                    (txexpr* 'tr empty
                             (txexpr 'td '((class "text-right pr-4")) '("Rows Examined:"))
                             (txexpr 'td empty (list (number->string (examined-table-row-count results))))))))

(module+ test
  (test-case "results-summary will create a div summarising the results of the table"
    (define result "<div class=\"p-3 m-2 bg-white rounded-md\"><h2 class=\"text-3xl\">Statistics</h2><table class=\"table-auto w-full\"><tr><td class=\"text-right pr-4\">Start Time:</td><td>1970-01-01 00:00:00</td></tr><tr><td class=\"text-right pr-4\">End Time:</td><td>2000-02-28 13:14:00</td></tr><tr><td class=\"text-right pr-4\">Rows Examined:</td><td>0</td></tr></table></div>")
    (check-equal? (xexpr->html (results-summary test-zero-record-table)) result)))

(define (row-table rows #:row-creator [row-table-body row-table-body])
  (txexpr* 'div '((class "p-3 m-2 bg-white rounded-md"))
           (txexpr 'h2 '((class "text-3xl")) (list "Results"))
           (txexpr* 'table '((class "table-auto w-full"))
                    (row-table-header)
                    (row-table-body rows))))

(module+ test
  (test-case "row-table will create a table presenting an empty result"
    (define result "<div class=\"p-3 m-2 bg-white rounded-md\"><h2 class=\"text-3xl\">Results</h2><table class=\"table-auto w-full\"><thead><tr><th>Key</th><th>Rule</th></tr></thead><tbody><tr class=\"border-b-2 border-gray-300\"><td>No rows were examined</td></tr></tbody></table></div>")
    (define test-row empty)
    (check-equal? (xexpr->html (row-table test-row)) result))
  (test-case "row-table will create a table presenting the result"
    (define result "<div class=\"p-3 m-2 bg-white rounded-md\"><h2 class=\"text-3xl\">Results</h2><table class=\"table-auto w-full\"><thead><tr><th>Key</th><th>Rule</th></tr></thead><tbody><tr class=\"border-b-2 border-gray-300\"><td class=\"text-center\">1</td><td><ul><li class=\"float-left p-1 m-4 border-2 rounded-full\"><span class=\"p1\">email address</span></li><li class=\"float-left p-1 m-4 border-2 rounded-full\"><span class=\"p1\">AU phone number</span></li></ul></td></tr><tr class=\"border-b-2 border-gray-300\"><td class=\"text-center\">2</td><td><ul><li class=\"float-left p-1 m-4 border-2 rounded-full\"><span class=\"p1\">email address</span></li><li class=\"float-left p-1 m-4 border-2 rounded-full\"><span class=\"p1\">AU phone number</span></li></ul></td></tr></tbody></table></div>")
    (define test-rows (list  (examined-row (hash "key" '(1))
                                           '((1 "email address") (1 "AU phone number")))
                             (examined-row (hash "key" '(2))
                                           '((1 "email address") (1 "AU phone number")))))
    (check-equal? (xexpr->html (row-table test-rows)) result))
  (test-case "row-table calls the row-creator"
    (define test-rows '())
    (define mock-result (txexpr 'tr empty (list "test")))
    (define row-creator-mock (mock #:behavior (const mock-result)))
    (row-table test-rows #:row-creator row-creator-mock)
    (check-mock-called-with?  row-creator-mock (arguments test-rows))))

(define (row-table-header)
  (txexpr* 'thead
           empty
           (txexpr* 'tr empty
                   (txexpr 'th empty (list "Key"))
                   (txexpr 'th empty (list "Rule")))))

(module+ test
  (test-case "row-table will create a table presenting the result"
    (define result "<thead><tr><th>Key</th><th>Rule</th></tr></thead>")
    (check-equal? (xexpr->html (row-table-header)) result)))

(define (row-table-body rows)
  (if (empty? rows)
      (txexpr* 'tbody empty
               (txexpr* 'tr '((class "border-b-2 border-gray-300"))
                        (txexpr 'td empty '("No rows were examined"))))
      (cons 'tbody (map (lambda (row)
                          (txexpr* 'tr '((class "border-b-2 border-gray-300"))
                                   (txexpr 'td '((class "text-center"))
                                           (list (key->string
                                                  (hash-ref (examined-row-id row) "key"))))
                                   (txexpr* 'td empty
                                            (rule-list (examined-row-results row))))) rows))))

(module+ test
  (test-case "row-table-body will return a defult message if the row list is empty"
    (define result '(tbody (tr ((class "border-b-2 border-gray-300")) (td "No rows were examined"))))
    (define no-rows empty)
    (check-equal? (row-table-body no-rows) result))
  (test-case "row-table-body will create a data row with a single examined-row"
    (define result '(tbody (tr ((class "border-b-2 border-gray-300")) (td ((class "text-center")) "1") (td (ul (li ((class "float-left p-1 m-4 border-2 rounded-full")) (span ((class "p1")) "email address"))
                                                                                                               (li ((class "float-left p-1 m-4 border-2 rounded-full")) (span ((class "p1")) "AU phone number")))))))
    (define test-row (list (examined-row (hash "key" '(1))
                                         '((1 "email address") (1 "AU phone number")))))
    (check-equal? (row-table-body test-row) result))
  (test-case "row-table-body will create a data row with a complex key"
    (define result '(tbody (tr ((class "border-b-2 border-gray-300")) (td ((class "text-center")) "1, 2, three")
                               (td (ul (li ((class "float-left p-1 m-4 border-2 rounded-full")) (span ((class "p1")) "email address"))
                                       (li ((class "float-left p-1 m-4 border-2 rounded-full")) (span ((class "p1")) "AU phone number")))))))
    (define test-row (list (examined-row (hash "key" '(1 2 "three"))
                                         '((1 "email address") (1 "AU phone number")))))
    (check-equal? (row-table-body test-row) result))
  (test-case "row-table-body will create multiple data rows with multiple examined-rows"
    (define result '(tbody
                     (tr ((class "border-b-2 border-gray-300")) (td ((class "text-center")) "1") (td (ul (li ((class "float-left p-1 m-4 border-2 rounded-full")) (span ((class "p1")) "email address"))
                                                                                                         (li ((class "float-left p-1 m-4 border-2 rounded-full")) (span ((class "p1")) "AU phone number")))))
                     (tr ((class "border-b-2 border-gray-300")) (td ((class "text-center")) "2") (td (ul (li ((class "float-left p-1 m-4 border-2 rounded-full")) (span ((class "p1")) "email address"))
                                                                                                         (li ((class "float-left p-1 m-4 border-2 rounded-full")) (span ((class "p1")) "AU phone number")))))))
    (define test-rows (list  (examined-row (hash "key" '(1))
                                           '((1 "email address") (1 "AU phone number")))
                             (examined-row (hash "key" '(2))
                                           '((1 "email address") (1 "AU phone number")))))
    (check-equal? (row-table-body test-rows) result)))

(define (key->string key)
  (string-join (map (lambda (part-key) (format "~a" part-key)) key) ", "))

(module+ test
  (test-case "key->string returns a single number as a string"
    (define test-key '(1))
    (define result "1")
    (check-equal? (key->string test-key) result))
  (test-case "key->string returns a mixture of numbers as a string"
    (define test-key '(1 2.0))
    (define result "1, 2.0")
    (check-equal? (key->string test-key) result))
  (test-case "key->string returns a mixture of numbers and strings as a string"
    (define test-key '(1 "a" 2.1))
    (define result "1, a, 2.1")
    (check-equal? (key->string test-key) result)))

(define (rule-list rules)
  (if (empty? rules)
      (txexpr 'p empty '("No rules to display."))
      (txexpr 'ul empty
              (for/list ([rule rules])
                (quasiquote (li ((class "float-left p-1 m-4 border-2 rounded-full")) (span ((class "p1")) (unquote (cadr rule)))))))))

(module+ test
  (test-case "rule-list will return a default message if the rule list is empty"
    (define result "<p>No rules to display.</p>")
    (define no-rules empty)
    (check-equal? (xexpr->html (rule-list no-rules)) result))
  (test-case "rule-list will return an unordered list of each rule"
    (define result "<ul><li class=\"float-left p-1 m-4 border-2 rounded-full\"><span class=\"p1\">email address</span></li><li class=\"float-left p-1 m-4 border-2 rounded-full\"><span class=\"p1\">AU phone number</span></li></ul>")
    (define two-rules '((1 "email address") (1 "AU phone number")))
    (check-equal? (xexpr->html (rule-list two-rules)) result)))

(define (initial-html-summary-report)
  (define report (txexpr* 'html '((lang "en") (class "no-js"))
                          (txexpr* 'head empty
                                   (txexpr 'meta '((charset "UTF-8")))
                                   (txexpr 'meta '((name "viewport")
                                                   (content "width=device-width, initial-scale=1")))
                                   (txexpr 'title empty '("PII Spider Report"))
                                   (txexpr 'meta '((name "description")
                                                   (content "Report on PII discovered in this database"))))
                          (txexpr* 'body empty
                                   (txexpr 'h1 empty '("Summary of PII Spider report run"))
                                   (txexpr 'ul empty '("")))))
  (string-append "<!DOCTYPE html>" (xexpr->html report)))

(module+ test
  (test-case "initial-html-summary-report returns a HTML summary report"
    (define result "<!DOCTYPE html><html lang=\"en\" class=\"no-js\"><head><meta charset=\"UTF-8\"/><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"/><title>PII Spider Report</title><meta name=\"description\" content=\"Report on PII discovered in this database\"/></head><body><h1>Summary of PII Spider report run</h1><ul></ul></body></html>")
    (check-equal? (initial-html-summary-report) result)))

(define (save-html-summary-report #:output-dir [output-dir "output"]
                                  #:mkdir [make-directory* make-directory*]
                                  #:output-file [call-with-output-file call-with-output-file])
  (define report-location (string-append output-dir "/" "index.html"))
  (make-directory* output-dir)
  (define report (initial-html-summary-report))
  (call-with-output-file report-location 
    (lambda (out)
      (display report out)) #:exists 'replace)
  
  report-location)
(module+ test
  (define mock-outputter (lambda (filename output-proc #:exists exists-val)
                           (define mock-port (open-output-string))
                           (output-proc mock-port)
                           (get-output-string mock-port)))
  (define output-file-mock (mock #:behavior mock-outputter))
  (define-opaque test-mkdir)
  (define mkdir-mock (mock #:behavior (const test-mkdir)))

  (test-case "save=html-summary-report returns the rpoert location"
    (check-equal? (save-html-summary-report #:mkdir mkdir-mock #:output-file output-file-mock) "output/index.html"))
  (test-case "save=html-summary-report returns the rpoert location"
    (check-equal? (save-html-summary-report #:output-dir "test" #:mkdir mkdir-mock #:output-file output-file-mock) "test/index.html"))
  (test-case "save-html-report-summary will make the output directory"
    (save-html-summary-report #:mkdir mkdir-mock #:output-file output-file-mock)
    (check-mock-called-with? mkdir-mock (arguments "output")))
  (test-case "save-html-report-summary saves the report output "
    (mock-reset! output-file-mock)
    (save-html-summary-report #:mkdir mkdir-mock #:output-file output-file-mock)
    (define result (car (mock-call-results (car (mock-calls output-file-mock)))))
    (check-equal? result (initial-html-summary-report))))

(define (update-html-summary-report table-name location
                                    #:input-file [open-input-file open-input-file]
                                    #:output-file [call-with-output-file call-with-output-file])
  (define summary-location "output/index.html")
  (define report
    (port->string (open-input-file summary-location) #:close? #t))
  (define list-item (string-append "<li><a href=\"" location "\">" table-name "</a></li>"))
  (define list-bottom (string-replace report "</ul>" (string-append list-item "</ul>")))

  (call-with-output-file summary-location
    (lambda (out)
      (display list-bottom out)) #:exists 'replace)
  #t)

(module+ test
  (define mock-report (lambda (filename) (open-input-string "<ul></ul>")))
  (define input-file-mock (mock #:behavior mock-report))

  (test-case "update-html-summary-report opens index.html"
    (update-html-summary-report "test" "test.html" #:input-file input-file-mock #:output-file output-file-mock)
    (check-mock-called-with? input-file-mock (arguments "output/index.html")))
  (test-case "update-html-summary-report writes index.html but only once"
    (mock-reset! input-file-mock)
    (mock-reset! output-file-mock)
    (update-html-summary-report "test" "test.html" #:input-file input-file-mock #:output-file output-file-mock)
    (check-mock-num-calls output-file-mock 1))
  (test-case "update-html-summary-report puts the report back"
    (mock-reset! input-file-mock)
    (mock-reset! output-file-mock)
    (define expectation "<ul><li><a href=\"test.html\">test</a></li></ul>")
    (update-html-summary-report "test" "test.html" #:input-file input-file-mock #:output-file output-file-mock)
    (define result (car (mock-call-results (car (mock-calls output-file-mock)))))
    (check-equal? result expectation)))

(define (save-report examined-table-record #:output-dir [output-dir "output"]
                     #:html-report [html-table-report html-table-report]
                     #:save-file [call-with-output-file call-with-output-file]
                     #:mkdir [make-directory* make-directory*])
  (define report (html-table-report examined-table-record))
  (make-directory* output-dir)
  (define output-file-name (string-append output-dir "/"
                                          (examined-table-name examined-table-record) ".html"))
  (call-with-output-file output-file-name
    #:exists 'truncate
    (lambda (out)
      (display report out)))
  output-file-name)

(module+ test
  (define examined-table-result (examined-table "two_rows" start-time end-time 2 test-two-rows))
  (define test-report "HTML report")
  (define report-mock (mock #:behavior (const test-report)))
  
  (test-case "save-report returns the name of the file it saved if it works"
    (check-equal? (save-report examined-table-result #:save-file output-file-mock
                               #:mkdir mkdir-mock) "output/two_rows.html"))
  (test-case "save-report returns the name of the file it saved if the output-dir is specified"
    (check-equal?
     (save-report examined-table-result #:output-dir "example" #:save-file output-file-mock #:mkdir mkdir-mock)
     "example/two_rows.html"))
  (test-case "save-report generates a HTML report via html-table-report"
    (mock-reset! output-file-mock)
    (save-report examined-table-result #:html-report report-mock
                 #:save-file output-file-mock #:mkdir mkdir-mock)
    (check-mock-called-with? report-mock (arguments examined-table-result)))
  (test-case "save-report saves the file"
    (mock-reset! output-file-mock)
    (save-report examined-table-result #:html-report report-mock
                 #:save-file output-file-mock #:mkdir mkdir-mock)
    (define result (car (mock-call-results (car (mock-calls output-file-mock)))))
    (check-equal? result test-report)))
