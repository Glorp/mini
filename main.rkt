#lang racket/base

(require racket/match
         racket/list
         racket/string
         racket/format
         racket/file
         web-server/servlet
         web-server/servlet-env
         "day.rkt"
         "post.rkt"
         "repo.rkt"
         "html.rkt"
         "down.rkt")

(define css (file->string "./style.css"))

(define the-day (today))
(define r (open-repo 'memory))
(define t (topic 'welp "Welperson"))
(create-topic r t)
(create-post r (post (add-days the-day -5) "blah bla\nbla\n\nblep" 'welp "https://garoof.no"))
(create-post r (post (add-days the-day -3) "beep\nboop\nbap" #f "https://garoof.no"))
(create-post r (post (add-days the-day -1) "okay" #f #f))

(define (out-html htmlx)
  (λ (out)
    (write-string doctype out)
    (write-html htmlx out)))

(define (ok title body)
  (response/output (out-html (page title body))))

(define (page title body)
  `(html
    ([lang "en"])
    (head
     (meta ([charset "utf-8"]))
     (title ,title)
     (link ([href "/style.css"] [rel "stylesheet"])))
    ,body))

(define (not-found)
  (define htmlx (page "404 Not Found :(" '(body (h1 "Not found :("))))
  (response 404 #"Not Found" (current-seconds) TEXT/HTML-MIME-TYPE '() (out-html htmlx)))

(define (sea-otter path)
  (define headers (list (header #"location" (string->bytes/utf-8 path))))
  (response 303 #"Sea Otter" (current-seconds) #f headers (λ (out) (void))))

(define (bad body)
  (define htmlx (page "400 Bad Request :(" body))
  (response 400 #"Not Found" (current-seconds) TEXT/HTML-MIME-TYPE '() (out-html htmlx)))

(define index '(a ([href "/index.html"]) "back to index"))

(define mdr #px"^(\\d\\d)-(\\d\\d)$")
(define yr #px"^(\\d\\d\\d\\d)$")

(define (pad n w)
  (~a n #:min-width w #:align 'right #:pad-string "0"))
(define (day->link dy)
  (match dy
    [(day y m d) (format "/~a/~a/~a" (pad y 4) (pad m 2) (pad d 2))]))

(define (link-tr link)
  (if link
      `((tr (th "Links to:") (td (a ([href ,link]) ,link))))
      '()))

(define (thread-tr tp)
  (match tp
    [(topic id name)
     (define id-str (symbol->string id))
     `((tr (th "In thread:")
            (td (a ([href ,(format "/topic/~a" id-str)]) ,name))))]
    [#f '()]))

(define (html-section day text rows)
  (define str (day->string day))
  `(section
    ([id ,str])
    (h2 (a ([href ,(day->link day)]) ,str))
    ,@(parsedown text)
    ,@(if (empty? rows)
          '()
          `((footer (table ,@rows))))))

(define (post->section-in-thread p)
  (match p
    [(post day text _ link) (html-section day text (link-tr link))]))

(define (post->section-no-thread p)
  (match p
    [(post day text id link)
     (html-section day text `(,@(thread-tr (and id (get-topic r id))) ,@(link-tr link)))]))

(define (lfstring str)
  (regexp-replace* #px"\r\n?" str "\n"))

(define (servlet req)
  (define bindings (request-bindings req))
  (match* ((request-method req) (map path/param-path (url-path (request-uri req))))
    [(#"GET" (or '("") '("index.html")))
     (match-define (day y m d) the-day)
     (define (pad v n)
       (~a v #:width n #:align 'right #:left-pad-string "0"))
     (define posts (get-posts r 'desc (before the-day)))
     (ok "miniblag"
         `(body
           (h1 "an miniature weblog")
           (p "today really is " ,(day->string the-day))
           (form ([action "next-day"] [method "post"])
                 (input ([type "submit"] [value "Next day plox"])))
           (form
            ([action ,(format "~a/~a-~a/post" (pad y 4) (pad m 2) (pad d 2))] [method "post"])
            (label "Text: " (textarea ([name "text"])))
            (label "Optionally a link: " (input ([name "link"])))
            (input ([type "submit"] [value "Submit"])))
           (p "posts:")
           ,@(map post->section-no-thread posts)))]
    [(#"GET" (list (regexp #px"\\d+" `(,y)) (regexp #px"\\d+" `(,m)) (regexp #px"\\d+" `(,d))))
     (define dy (maybe-day (string->number y) (string->number m) (string->number d)))
     (printf "~a" dy)
     (define p (and dy (get-post r dy)))
     (if p
         (ok (day->string dy) `(body ,(post->section-in-thread p)))
         (not-found))]
    [(#"POST" (list (regexp yr (list _ y)) (regexp mdr (list _ m d)) "post"))
     (match bindings
       [`((text . ,crlftext) (link . ,linktext))
        (define trimmed (string-trim linktext))
        (define link (and (non-empty-string? trimmed) trimmed))
        (define text (lfstring crlftext))
        (define dy (maybe-day (string->number y) (string->number m) (string->number d)))
        (cond [(not dy) (not-found)]
              [(not (valid-text? text))
               (match-define (list ok too-much) (valid-text-split text))
               (bad `(body
                      (h1 "text too long >:(")
                      (p "okay: " ,(~s ok))
                      (p "too much: " ,(~s too-much))))]
              [(get-post r dy) (bad `(body (h1 "there's already a post for" (day->string dy))))]
              [else (create-post r (post dy text #f link))
                    (ok "success :)"
                        `(body
                          (h1 "nice :)")
                          (p "created post for " ,(day->string dy))
                          (p ,index)))])]
       [_ (bad `(body
                 (h1 "bad parameters")
                 (p (~s bindings) " <- what is ??")
                 (p "want: param text then param link")))])]
    [(#"POST" (list "next-day"))
     (set! the-day (add-days the-day 1))
     (sea-otter "/index.html")]
    [(#"GET" (list "style.css"))
     (response/output (λ (out) (write-string css out)) #:mime-type #"text/css; charset=utf-8")]
    [(method path)
     (printf "not found: ~a, ~a, ~s~n" method path bindings)
     (not-found)]))

(serve/servlet
 servlet
 #:stateless? #t
 #:servlet-regexp #rx""
 #:servlet-path "/index.html#2026-01-10"
 #:extra-files-paths
               (list
                (build-path (current-directory) "static")))


