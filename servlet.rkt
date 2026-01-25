#lang racket/base

(require (only-in racket/match match match* match-define)
         (only-in racket/string non-empty-string? string-trim)
         (only-in racket/format ~a ~s)
         (only-in racket/file file->string)
         (only-in web-server/servlet
                  response
                  response/output
                  request-method
                  request-bindings
                  path/param-path
                  header
                  url-path
                  request-uri
                  make-basic-auth-header
                  request->basic-credentials
                  TEXT/HTML-MIME-TYPE)
         "day.rkt"
         "post.rkt"
         "repo.rkt"
         "pages.rkt"
         "write-html.rkt"
         "html.rkt")

(provide servlet)

(define css (file->string "./style.css"))

(define (out-html html)
  (λ (out)
    (write-html html out)))

(define (old-ok user title content)
  (response/output (out-html (page user title content))))

(define (ok page)
  (response/output (out-html page)))

(define (not-found user)
  (let ([htmlx (page user "404 Not Found :(" '((h1 "Not found :(")))])
    (response 404 #"Not Found" (current-seconds) TEXT/HTML-MIME-TYPE '() (out-html htmlx))))

(define (sea-otter path)
  (define headers (list (header #"location" (string->bytes/utf-8 path))))
  (response 303 #"Sea Otter" (current-seconds) #f headers (λ (out) (void))))

(define (bad user content)
  (define htmlx (page user "400 Bad Request :(" content))
  (response 400 #"Not Found" (current-seconds) TEXT/HTML-MIME-TYPE '() (out-html htmlx)))

(define mdr #px"^(\\d\\d)-(\\d\\d)$")
(define mdr-dot-html #px"^(\\d\\d)-(\\d\\d)[.]html$")
(define yr #px"^(\\d\\d\\d\\d)$")
(define mr-dot-html #px"^(\\d\\d)[.]html$")

(define (lfstring str)
  (regexp-replace* #px"\r\n?" str "\n"))

(define (strings->maybe-day y m d)
  (maybe-day (string->number y) (string->number m) (string->number d)))

(define unauthorized
  (response 401 #"Unauthorized" 
            (current-seconds) 
            TEXT/HTML-MIME-TYPE
            (list (make-basic-auth-header "Authentication required"))
            void))

(define (maybe-login req login?)
  (match (request->basic-credentials req)
    [#f #f]
    [(cons userbytes pwdbytes)
     (and (login? userbytes pwdbytes) (bytes->string/utf-8 userbytes))]))


(define per-page 2)
  

(define (servlet r login? get-today)
  (λ (req)
    (define the-day (get-today))
    (define user (maybe-login req login?))
    (define method (request-method req))
    (define path (map path/param-path (url-path (request-uri req))))
    
    (match* (method user)
      [(#"POST" #f) unauthorized]
      [(_ _)
       (define bindings (request-bindings req))

       (define (store-post dy store)
         (match bindings
           [`((text . ,crlftext) (symbol . ,symtext) (link . ,linktext))
            (define symbol (and (non-empty-string? (string-trim symtext))
                                (string->symbol symtext)))
            (define tp (and symbol (get-topic r symbol)))
            (define trimmed (string-trim linktext))
            (define link (and (non-empty-string? trimmed) trimmed))
            (define text (lfstring crlftext))
            (cond [(not dy) (not-found user)]
                  [(not (valid-text? text))
                   (match-define (list ok too-much) (valid-text-split text))
                   (bad user
                        `((h1 "text too long >:(")
                          (p "okay: " ,(~s ok))
                          (p "too much: " ,(~s too-much))))]
                  [(and symbol (not tp))
                   (bad user `((h1 "\"" ,symtext "\" does not refer to an existing topic")))]
                  [else (store r (post dy text symbol link))
                        (sea-otter (day->url dy "/edit"))])]
           [_ (bad user
                   `((h1 "bad parameters")
                     (p ,(~s bindings) " <- what is ??")
                     (p "want: param text then param link")))]))

       (define (day-handler dy rest)
         (define dstr (day->string dy))
         (define p (get-post r dy))
         (match* (method rest)
           [(#"GET" '())
            (ok (day-page r user dy))]
           [(#"GET" '("edit"))
            (ok (edit-post-page r user dy))]
           [(#"POST" '("create"))
            (if (get-post r dy)
                (bad user `((h1 "there's already a post for " (time ,dstr))))
                (store-post dy create-post))]
           [(#"POST" '("update"))
            (if (get-post r dy)
                (store-post dy update-post)
                (bad user `((h1 "there's no post for " ,dstr " to edit. hmm!"))))]
           [(#"POST" '("delete"))
            (match (get-post r dy)
              [#f (bad user `((h1 "there's no post for " ,dstr " to edit. hmm!")))]
              [p (delete-post r p)
                 (sea-otter (day->url dy "/edit"))])]
           [(#"POST" '("tag")) (tag-handler dy tag)]
           [(#"POST" '("untag")) (tag-handler dy untag)]
           [(method path)
            (printf "not found: ~a ~a, ~a, ~s~n" dy method path bindings)
            (not-found user)]))

       (define (tag-handler dy tag/untag)
         (match (get-post r dy)
           [#f (bad user `((h1 "there's no post for " ,(day->string dy) " to tag. hmm!")))]
           [p (match bindings
                [(list (cons sym _))
                 (match (hash-ref (topics-hash (all-topics r)) sym #f)
                   [#f (bad user `((h1 "bad params. hmm!")))]
                   [tp (tag/untag r p tp)
                       (sea-otter (day->url dy "/edit"))])]
                [_ (bad user `((h1 "bad params. hmm!")))])]))
       
       (match* (method path)
         [(#"GET" '("login")) (if user (sea-otter "/index.html") unauthorized)]
         [(#"GET" (or '("") '("index.html"))) (ok (index r user the-day))]
         [(method (list (regexp yr (list _ y)) (regexp mdr (list _ m d)) rest ...))
          (match (maybe-day (string->number y) (string->number m) (string->number d))
            [#f (not-found user)]
            [dy (day-handler dy rest)])]
         [(#"GET" (list (regexp yr (list _ y)) (regexp mdr-dot-html (list _ m d))))
          (match (maybe-day (string->number y) (string->number m) (string->number d))
            [#f (not-found user)]
            [dy (ok (day-page r user dy))])]
         [(#"GET" (list "archive.html")) (ok (archive-page r user the-day))]
         [(#"GET" (list (regexp yr (list _ y)) (regexp mr-dot-html (list _ m))))
          (define dy (maybe-day (string->number y) (string->number m) 1))
          (match dy
            [#f (not-found user)]
            [(day y m _) (ok (month-page r user y m))])]
         [(#"GET" (list "topics.html")) (ok (topics-page r user))]
         [(#"GET" (list "topic" (regexp #px"^(.+?)[.]html$" (list _ str))))
          (match (get-topic r (string->symbol str))
            [#f (not-found user)]
            [tp (ok (topic-page r user tp))])]
         [(#"POST" (list "topic" str "update"))
          (define sym (string->symbol str))
          (define tp (get-topic r sym))
          (match tp
            [#f (not-found user)]
            [_ (match bindings
                 [`((name . ,name) (type . ,typestr))
                  (update-topic r (topic sym name (string->symbol typestr)))
                  (sea-otter (symbol->url sym ".html"))]
                 [_ (bad user '((h1 "Bad Request") (p "bad params :(")))])])]
         [(#"POST" (list "topic" str "delete"))
          (define sym (string->symbol str))
          (define tp (get-topic r sym))
          (match tp
            [#f (not-found user)]
            [_ (delete-topic r tp)
               (sea-otter "/topics.html")])]
         [(#"POST" (list "topics" "create"))
          (match bindings
            [`((symbol . ,symstr) (name . ,name) (type . ,typestr))
             (define sym (string->symbol symstr))
             (define type (string->symbol typestr))
             (cond [(get-topic r sym)
                    (bad user '((h1 "Bad Request") (p "Topic " ,symstr " already exists")))]
                   [else
                    (create-topic r (topic sym name type))
                    (sea-otter (symbol->url sym ".html"))])]
            [_ (bad user '((h1 "Bad Request") (p "bad params :(")))])]
         [(#"GET" (list "style.css"))
          (response/output (λ (out) (write-string css out)) #:mime-type #"text/css; charset=utf-8")]
         [(method path)
          (printf "not found: ~a, ~a, ~s~n" method path bindings)
          (not-found user)])])))
