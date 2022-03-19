#lang racket/base

(require racket/function)
(require rackunit)
(require mock)
(require mock/rackunit)
(require txexpr)
(require gregor)
(require pii_spider/reports/reports)
(require pii_spider/structs)

(define start-time (moment 1970))
(define end-time (moment 2000 02 28 13 14 30))
(define test-no-rows null)
(define test-two-rows (list  (examined-row (hash "key" '(1))
                                           '((1 "email address") (1 "AU phone number")))
                             (examined-row (hash "key" '(2))
                                           '((1 "email address") (1 "AU phone number"))))) 
(define test-zero-record-table (examined-table "no_rows" start-time end-time 0 test-no-rows #f))
(define test-two-record-table (examined-table "two_rows" start-time end-time 2 test-two-rows #f))
(define mock-outputter (lambda (filename output-proc #:exists exists-val)
                           (define mock-port (open-output-string))
                           (output-proc mock-port)
                           (get-output-string mock-port)))
(define output-file-mock (mock #:behavior mock-outputter))
(define-opaque test-mkdir)
(define mkdir-mock (mock #:behavior (const test-mkdir)))
(define mock-report (lambda (filename) (open-input-string "<table></table>")))
(define input-file-mock (mock #:behavior mock-report))
(define ignore-nothing (ignore null (hasheq) (hasheq)))
(define examined-table-result (examined-table "two_rows" start-time end-time 2 test-two-rows #f))
(define test-report "HTML report")
(define report-mock (mock #:behavior (const test-report)))

(define reports-tests
  (test-suite
   "reports"
   (test-suite "initial-html-summary-report"
               (test-case "returns a HTML summary report"
                 (define result "<!DOCTYPE html><html lang=\"en\" class=\"no-js\"><head><meta charset=\"UTF-8\"/><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"/><title>PII Spider Report</title><meta name=\"description\" content=\"Report on data discovered in this database\"/><link href=\"https://unpkg.com/tailwindcss@^1.0/dist/tailwind.min.css\" rel=\"stylesheet\"/></head><body><div class=\"min-w-screen min-h-screen bg-gray-200 flex-col p-3 overflow-auto\"><div class=\"p-3 m-2 bg-white rounded-md\"><h1 class=\"text-center text-5xl font-extrabold\">Summary of PII Spider report run</h1></div><div class=\"p-3 m-2 bg-white rounded-md\"><table class=\"table-auto w-full\"></table></div></div></body></html>")
                 (check-equal? (initial-html-summary-report) result)))
   (test-suite "html-table-report"
               (test-case "produces a HTML report of the run"
                 (define result "<!DOCTYPE html><html lang=\"en\" class=\"no-js\"><head><meta charset=\"UTF-8\"/><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"/><title>PII Spider Report</title><meta name=\"description\" content=\"Report on data discovered in this database\"/><link href=\"https://unpkg.com/tailwindcss@^1.0/dist/tailwind.min.css\" rel=\"stylesheet\"/></head><body><div class=\"min-w-screen min-h-screen bg-gray-200 flex-col p-3 overflow-auto\"><h1 class=\"text-center text-5xl font-extrabold\">Report for table no_rows</h1><p>mock summary</p><p>mock table</p></div></body></html>")
                 
                 (define table-creator-mock (mock #:behavior (const (txexpr 'p empty (list "mock table")))))
                 (define summary-mock (mock #:behavior (const (txexpr 'p empty (list "mock summary")))))
                 (check-equal? (html-table-report test-zero-record-table #:table-creator table-creator-mock #:summary-creator summary-mock) result))
               (test-case "html-table-report calls the table-creator"
                 (define table-creator-mock (mock #:behavior (const (txexpr 'p empty (list "mock table")))))
                 (define test-zero-record-table (examined-table "no_rows" start-time end-time 0 test-no-rows #f))                 
                 (html-table-report test-zero-record-table #:table-creator table-creator-mock)
                 (check-mock-called-with?  table-creator-mock (arguments (examined-table-results test-zero-record-table))))
               (test-case "html-table-report produces a report for the table"
                 (define result "<!DOCTYPE html><html lang=\"en\" class=\"no-js\"><head><meta charset=\"UTF-8\"/><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"/><title>PII Spider Report</title><meta name=\"description\" content=\"Report on data discovered in this database\"/><link href=\"https://unpkg.com/tailwindcss@^1.0/dist/tailwind.min.css\" rel=\"stylesheet\"/></head><body><div class=\"min-w-screen min-h-screen bg-gray-200 flex-col p-3 overflow-auto\"><h1 class=\"text-center text-5xl font-extrabold\">Report for table two_rows</h1><div class=\"p-3 m-2 bg-white rounded-md\"><h2 class=\"text-3xl\">Statistics</h2><table class=\"table-auto w-full\"><tr><td class=\"text-right pr-4\">Start Time:</td><td>1970-01-01 00:00:00</td></tr><tr><td class=\"text-right pr-4\">End Time:</td><td>2000-02-28 13:14:00</td></tr><tr><td class=\"text-right pr-4\">Rows Examined:</td><td>2</td></tr></table></div><div class=\"p-3 m-2 bg-white rounded-md\"><h2 class=\"text-3xl\">Results</h2><table class=\"table-auto w-full\"><thead><tr><th>Key</th><th>Rule</th></tr></thead><tbody><tr class=\"border-b-2 border-gray-300\"><td class=\"text-center\">1</td><td><ul><li class=\"float-left p-1 m-4 border-2 rounded-full\"><span class=\"p1\">email address</span></li><li class=\"float-left p-1 m-4 border-2 rounded-full\"><span class=\"p1\">AU phone number</span></li></ul></td></tr><tr class=\"border-b-2 border-gray-300\"><td class=\"text-center\">2</td><td><ul><li class=\"float-left p-1 m-4 border-2 rounded-full\"><span class=\"p1\">email address</span></li><li class=\"float-left p-1 m-4 border-2 rounded-full\"><span class=\"p1\">AU phone number</span></li></ul></td></tr></tbody></table></div></div></body></html>")
                 (check-equal? (html-table-report test-two-record-table) result)))
   (test-suite "results-summary"
               (test-case "results-summary will create a div summarising the results of the table"
                 (define result "<div class=\"p-3 m-2 bg-white rounded-md\"><h2 class=\"text-3xl\">Statistics</h2><table class=\"table-auto w-full\"><tr><td class=\"text-right pr-4\">Start Time:</td><td>1970-01-01 00:00:00</td></tr><tr><td class=\"text-right pr-4\">End Time:</td><td>2000-02-28 13:14:00</td></tr><tr><td class=\"text-right pr-4\">Rows Examined:</td><td>0</td></tr></table></div>")
                 (check-equal? (xexpr->html (results-summary test-zero-record-table)) result)))
   (test-suite "row-table"
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
                 (check-mock-called-with?  row-creator-mock (arguments test-rows)))
               )
   (test-suite "save-html-summary-report"
               (test-case "returns the report location"
                 (check-equal? (save-html-summary-report #:mkdir mkdir-mock #:output-file output-file-mock) "output/index.html"))
               (test-case "returns the report location"
                 (check-equal? (save-html-summary-report #:output-dir "test" #:mkdir mkdir-mock #:output-file output-file-mock) "test/index.html"))
               (test-case "will make the output directory"
                 (save-html-summary-report #:mkdir mkdir-mock #:output-file output-file-mock)
                 (check-mock-called-with? mkdir-mock (arguments "output")))
               (test-case "saves the report output "
                 (mock-reset! output-file-mock)
                 (save-html-summary-report #:mkdir mkdir-mock #:output-file output-file-mock)
                 (define result (car (mock-call-results (car (mock-calls output-file-mock)))))
                 (check-equal? result (initial-html-summary-report))))
   (test-suite "update-html-summary-report"
               (test-case "update-html-summary-report opens index.html"
                 (update-html-summary-report "test" "test.html" ignore-nothing #:input-file input-file-mock #:output-file output-file-mock)
                 (check-mock-called-with? input-file-mock (arguments "output/index.html")))
               (test-case "update-html-summary-report writes index.html but only once"
                 (mock-reset! input-file-mock)
                 (mock-reset! output-file-mock)
                 (update-html-summary-report "test" "test.html" ignore-nothing #:input-file input-file-mock #:output-file output-file-mock)
                 (check-mock-num-calls output-file-mock 1))
               (test-case "update-html-summary-report writes in a table row for table"
                 (mock-reset! input-file-mock)
                 (mock-reset! output-file-mock)
                 (define expectation "<table><tr><td><a href=\"test.html\">test</a></td><td></td></tr></table>")
                 (update-html-summary-report "test" "test.html" ignore-nothing #:input-file input-file-mock #:output-file output-file-mock)
                 (define result (car (mock-call-results (car (mock-calls output-file-mock)))))
                 (check-equal? result expectation))
               (test-case "update-html-summary-report writes in a table row for tables that are ignored"
                 (mock-reset! input-file-mock)
                 (mock-reset! output-file-mock)
                 (define ignore-test (ignore '("test") (hasheq) (hasheq)))
                 (define expectation "<table><tr><td><a href=\"test.html\">test</a></td><td class=\"float-right\">IGNORED</td></tr></table>")
                 (update-html-summary-report "test" "test.html" ignore-test #:input-file input-file-mock #:output-file output-file-mock)
                 (define result (car (mock-call-results (car (mock-calls output-file-mock)))))
                 (check-equal? result expectation)))
   (test-suite
    "save-report"
    (test-case "returns the name of the file it saved if it works"
      (define result (path->string (build-path (current-directory) "output/two_rows.html")))
      (check-equal? (save-report examined-table-result #:save-file output-file-mock
                                 #:mkdir mkdir-mock) result))
    (test-case "returns the name of the file it saved if the output-dir is specified"
      (define result (path->string (build-path (current-directory) "example/two_rows.html")))
      (check-equal?
       (save-report examined-table-result
                    #:output-dir (build-path (current-directory) "example")
                    #:save-file output-file-mock #:mkdir mkdir-mock)
       result))
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
   ))

(module+ test
  (require rackunit/text-ui)
  (run-tests reports-tests))
