#lang racket/base

(require racket/string)
(require racket/file)
(require racket/port)
(require txexpr)
(require gregor)
(require "../structs.rkt")

(module+ test
  (require rackunit)
  (require mock)
  (require mock/rackunit))

(provide html-table-report save-report save-html-summary-report update-html-summary-report
         initial-html-summary-report results-summary row-table)

(define (html-header)
  (txexpr* 'head empty
           (txexpr 'meta '((charset "UTF-8")))
           (txexpr 'meta '((name "viewport")
                           (content "width=device-width, initial-scale=1")))
           (txexpr 'title empty '("PII Spider Report"))
           (txexpr 'meta '((name "description")
                           (content "Report on data discovered in this database")))
           (txexpr 'link '((href "https://unpkg.com/tailwindcss@^1.0/dist/tailwind.min.css") (rel "stylesheet")))))
(module+ test
  (test-case "html-header makes a standard header including tailwind etc."
    (define expected-result "<head><meta charset=\"UTF-8\"/><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"/><title>PII Spider Report</title><meta name=\"description\" content=\"Report on data discovered in this database\"/><link href=\"https://unpkg.com/tailwindcss@^1.0/dist/tailwind.min.css\" rel=\"stylesheet\"/></head>")
    (check-equal? (xexpr->html (html-header)) expected-result)))

(define (html-table-report table-results #:table-creator [row-table row-table] #:summary-creator [results-summary results-summary])
  (define report (txexpr* 'html '((lang "en") (class "no-js"))
                          (html-header)
                          (txexpr* 'body empty
                                   (txexpr* 'div '((class "min-w-screen min-h-screen bg-gray-200 flex-col p-3 overflow-auto"))
                                            (txexpr 'h1 '((class "text-center text-5xl font-extrabold")) (list (string-append "Report for table " (examined-table-name table-results))))
                                            (results-summary table-results)
                                            (row-table (examined-table-results table-results))))))
  (string-append "<!DOCTYPE html>" (xexpr->html report)))


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


(define (row-table rows #:row-creator [row-table-body row-table-body])
  (txexpr* 'div '((class "p-3 m-2 bg-white rounded-md"))
           (txexpr 'h2 '((class "text-3xl")) (list "Results"))
           (txexpr* 'table '((class "table-auto w-full"))
                    (row-table-header)
                    (row-table-body rows))))

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
  (if (null? rows)
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
  (define triggered-rules (filter (lambda (rule) (< 0 (car rule))) rules))
  (if (null? triggered-rules)
      (txexpr 'p empty '("No rules to display."))
      (txexpr 'ul empty
              (for/list ([rule triggered-rules])
                (quasiquote (li ((class "float-left p-1 m-4 border-2 rounded-full")) (span ((class "p1")) (unquote (cadr rule)))))))))

(module+ test
  (test-case "rule-list will return a default message if the rule list is empty"
    (define result "<p>No rules to display.</p>")
    (define no-rules empty)
    (check-equal? (xexpr->html (rule-list no-rules)) result))
  (test-case "rule-list will return an unordered list of each rule"
    (define result "<ul><li class=\"float-left p-1 m-4 border-2 rounded-full\"><span class=\"p1\">email address</span></li><li class=\"float-left p-1 m-4 border-2 rounded-full\"><span class=\"p1\">AU phone number</span></li></ul>")
    (define two-rules '((1 "email address") (1 "AU phone number")))
    (check-equal? (xexpr->html (rule-list two-rules)) result))
  (test-case "rule-list will return an unordered list of each rule except rules that haven't been triggered"
    (define result "<ul><li class=\"float-left p-1 m-4 border-2 rounded-full\"><span class=\"p1\">email address</span></li></ul>")
    (define one-triggered-rule '((1 "email address") (0 "AU phone number")))
    (check-equal? (xexpr->html (rule-list one-triggered-rule)) result)))

(define (initial-html-summary-report)
  (define report (txexpr* 'html '((lang "en") (class "no-js"))
                          (html-header)
                          (txexpr* 'body empty
                                   (txexpr* 'div '((class "min-w-screen min-h-screen bg-gray-200 flex-col p-3 overflow-auto"))
                                            (txexpr* 'div '((class "p-3 m-2 bg-white rounded-md"))
                                                     (txexpr 'h1 '((class "text-center text-5xl font-extrabold")) '("Summary of PII Spider report run")))
                                            (txexpr* 'div '((class "p-3 m-2 bg-white rounded-md"))
                                                     (txexpr 'table '((class "table-auto w-full")) '("")))))))
  (string-append "<!DOCTYPE html>" (xexpr->html report)))

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

(define (update-html-summary-report table-name location ignores
                                    #:input-file [open-input-file open-input-file]
                                    #:output-file [call-with-output-file call-with-output-file])
  (define summary-location "output/index.html")
  (define report
    (port->string (open-input-file summary-location) #:close? #t))
  (define ignored-status (if (member table-name (ignore-tables ignores))
                             "<td class=\"float-right\">IGNORED</td>"
                             "<td></td>"))
  (define list-item (string-append "<tr><td><a href=\"" location "\">" table-name "</a></td>" ignored-status "</tr>"))
  (define list-bottom (string-replace report "</table>" (string-append list-item "</table>")))

  (call-with-output-file summary-location
    (lambda (out)
      (display list-bottom out)) #:exists 'replace)
  #t)

(define (save-report examined-table-record
                     #:output-dir [output-dir (build-path (current-directory) "output")]
                     #:html-report [html-table-report html-table-report]
                     #:save-file [call-with-output-file call-with-output-file]
                     #:mkdir [make-directory* make-directory*])
  (define report (html-table-report examined-table-record))
  (log-info (format "output directory for ~a is ~a"
                    (examined-table-name examined-table-record) (path->string output-dir)))
  (make-directory* output-dir)
  (define output-file-name (string-append (path->string output-dir) "/"
                                          (examined-table-name examined-table-record) ".html"))
  (call-with-output-file output-file-name
    #:exists 'truncate
    (lambda (out)
      (display report out)))
  output-file-name)
