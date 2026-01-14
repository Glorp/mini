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
         "down.rkt"
         "site.rkt")

(define css (file->string "./style.css"))

(define the-day (today))
(define r (open-repo 'memory))
(define t (topic 'important-topic "An important topic" 'thread))
(create-topic r t)
(create-post r (post (add-days the-day -10)
                     "lorem ipswitch\nbla _bli_ `blablah`\n\nboop."
                     'important-topic
                     "https://dailyotter.org"))
(create-post r (post (add-days the-day -5) "beep\nboop\nbap" #f "https://dailybunny.org"))
(create-post r (post (add-days the-day -3) "blep\nblop\nblap" 'important-topic #f))
(create-post r (post (add-days the-day -1) "mlep\nmlop\nmlap" #f #f))

(define (out-html html)
  (λ (out)
    (write-string doctype out)
    (write-html html out)))

(define (ok title body)
  (response/output (out-html (page title body))))

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

(define (lfstring str)
  (regexp-replace* #px"\r\n?" str "\n"))

(define (strings->maybe-day y m d)
  (maybe-day (string->number y) (string->number m) (string->number d)))

(define (store-post dy bindings store)
  (match bindings
       [`((text . ,crlftext) (link . ,linktext))
        (define trimmed (string-trim linktext))
        (define link (and (non-empty-string? trimmed) trimmed))
        (define text (lfstring crlftext))
        (cond [(not dy) (not-found)]
              [(not (valid-text? text))
               (match-define (list ok too-much) (valid-text-split text))
               (bad `(body
                      (h1 "text too long >:(")
                      (p "okay: " ,(~s ok))
                      (p "too much: " ,(~s too-much))))]
              [else (store r (post dy text #f link))
                    (ok "success :)"
                        `(body
                          (h1 "nice :)")
                          (p "created post for " ,(day->string dy))
                          (p ,index)))])]
       [_ (bad `(body
                 (h1 "bad parameters")
                 (p (~s bindings) " <- what is ??")
                 (p "want: param text then param link")))]))

(define (servlet req)
  (define bindings (request-bindings req))
  (match* ((request-method req) (map path/param-path (url-path (request-uri req))))
    [(#"GET" (or '("") '("index.html")))
     (define (pad v n)
       (~a v #:width n #:align 'right #:left-pad-string "0"))
     (define posts (get-posts r 'desc (before the-day)))
     (ok "Miniature weblog"
         `(body
           (h1 "Miniature weblog")
           (p "Today really is " ,(day->string the-day))
           (form ([action "next-day"] [method "post"])
                 (input ([type "submit"] [value "Next day plox"])))
           ,(day/post->form the-day (get-post r the-day))
           (p "posts:")
           ,@(map (λ (p) (post->section-no-thread p (all-topics r))) posts)))]
    [(#"GET" (list (regexp #px"\\d+" `(,y)) (regexp #px"\\d+" `(,m)) (regexp #px"\\d+" `(,d))))
     (define dy (maybe-day (string->number y) (string->number m) (string->number d)))
     (define p (and dy (get-post r dy)))
     (if p
         (ok (day->string dy) `(body ,(post->section-in-thread p)))
         (not-found))]
    [(#"POST" (list (regexp yr (list _ y)) (regexp mdr (list _ m d)) "create"))
     (define dy (strings->maybe-day y m d))
     (if (get-post r dy)
         (bad `(body (h1 "there's already a post for " (day->string dy))))
         (store-post dy bindings create-post))]
    [(#"POST" (list (regexp yr (list _ y)) (regexp mdr (list _ m d)) "edit"))
     (define dy (strings->maybe-day y m d))
     (if (get-post r dy)
         (store-post dy bindings update-post)
         (bad `(body (h1 "there's no post for " (day->string dy) " to edit. hmm!"))))]
    [(#"GET" (list "topics" str))
     (match (get-topic r (string->symbol str))
       [#f (not-found)]
       [(topic symbol name type)
        (ok name
            `(body
              (h1 ,name)
              ,@(map (λ (p) (post->section-in-thread p)) (get-posts r 'asc (in-thread symbol)))))])]
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


