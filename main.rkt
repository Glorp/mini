#lang racket/base

(require racket/match
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
(define p1 (post (add-days the-day -5) "beep\nboop\nbap" #f "https://dailybunny.org"))
(define p2 (post (add-days the-day -3) "blep\nblop\nblap" 'important-topic #f))
(define p3 (post (add-days the-day -2) "mlep\nmlop\nmlap" #f #f))
(define p4 (post (add-days the-day -1) "BEPP" 'beep #f))
(create-post r p1)
(create-post r p2)
(create-post r p3)
(create-post r p4)
(define tag1 (topic 'beep "Beep" 'tag))
(define tag2 (topic 'boop "Boop" 'tag))
(create-topic r tag1)
(create-topic r tag2)
(tag r p1 tag1)
(tag r p1 tag2)
(tag r p2 tag1)

(define (out-html html)
  (λ (out)
    (write-html html out)))

(define (ok title body)
  (response/output (out-html (page title body))))

(define not-found
  (let ([htmlx (page "404 Not Found :(" '(body (h1 "Not found :(")))])
    (response 404 #"Not Found" (current-seconds) TEXT/HTML-MIME-TYPE '() (out-html htmlx))))

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

(define unauthorized
  (response 401 #"Unauthorized" 
            (current-seconds) 
            TEXT/HTML-MIME-TYPE
            (list (make-basic-auth-header "Authentication required"))
            void))

(define (login req)
  (match (request->basic-credentials req)
    [#f #f]
    [(cons name pwd)
     (and (equal? pwd #"blapp") name)]))

(define (store-post dy bindings store)
  (match bindings
    [`((text . ,crlftext) (link . ,linktext))
     (define trimmed (string-trim linktext))
     (define link (and (non-empty-string? trimmed) trimmed))
     (define text (lfstring crlftext))
     (cond [(not dy) not-found]
           [(not (valid-text? text))
            (match-define (list ok too-much) (valid-text-split text))
            (bad `(body
                   (h1 "text too long >:(")
                   (p "okay: " ,(~s ok))
                   (p "too much: " ,(~s too-much))))]
           [else (store r (post dy text #f link))
                 (sea-otter (~a (day->url dy) "/edit"))])]
    [_ (bad `(body
              (h1 "bad parameters")
              (p (~s bindings) " <- what is ??")
              (p "want: param text then param link")))]))

(define (day-handler method dy rest bindings)
  (define dstr (day->string dy))
  (define p (and dy (get-post r dy)))
  (match* (method rest)
    [(#"GET" '())
     (define symbol (and p (post-symbol p)))
     (cond [symbol (sea-otter (format "/topics/~a#~a" symbol dstr))]
           [p (ok (day->string dy) `(body ,(post->section-in-thread p (tags-hash r dy))))]
           [else not-found])]
    [(#"GET" '("edit"))
     (ok dstr
         `(body
           (h1 ,dstr)
           ,@(cond [p (define topics (all-topics r))
                      (define tags (tags-hash r dy))
                      `((p "Existing post:")
                        ,(post->section p topics tags)
                        ,@(tag-forms dy topics tags))]
                   [else `((p "There's no existing post for " (time ,dstr) "."))])
              ,(day/post->form dy p)))]
    [(#"POST" '("create"))
     (if (get-post r dy)
         (bad `(body (h1 "there's already a post for " (time ,dstr))))
         (store-post dy bindings create-post))]
    [(#"POST" '("update"))
     (if (get-post r dy)
         (store-post dy bindings update-post)
         (bad `(body (h1 "there's no post for " ,dstr " to edit. hmm!"))))]
    [(#"POST" '("update"))
     (if (get-post r dy)
         (store-post dy bindings update-post)
         (bad `(body (h1 "there's no post for " ,dstr " to edit. hmm!"))))]
    [(#"POST" '("tag")) (tag-handler dy bindings tag)]
    [(#"POST" '("untag")) (tag-handler dy bindings untag)]
    [(method path)
     (printf "not found: ~a ~a, ~a, ~s~n" dy method path bindings)
     not-found]))

(define (tag-handler dy bindings tag/untag)
  (match (get-post r dy)
    [#f (bad `(body (h1 "there's no post for " ,(day->string dy) " to tag. hmm!")))]
    [p (match bindings
         [(list (cons symbol _))
          (match (hash-ref (topics-hash (all-topics r)) symbol #f)
            [#f (bad `(body (h1 "bad params. hmm!")))]
            [tp (tag/untag r p tp)
                (sea-otter (~a (day->url dy) "/edit"))])]
         [_ (bad `(body (h1 "bad params. hmm!")))])]))

(define (servlet req)
  (define bindings (request-bindings req))
  (match* ((request-method req) (map path/param-path (url-path (request-uri req))))
    [(#"GET" '("login"))
     (if (login req)
         (sea-otter "/index.html")
         unauthorized)]
    [(#"GET" (or '("") '("index.html")))
     (define posts (get-posts r 'desc (before the-day)))
     (define tags (apply tags-hash r (map post-day posts)))
     (ok "Miniature weblog"
         `(body
           (h1 "Miniature weblog")
           (p "Today really is " ,(day->string the-day))
           (form ([action "next-day"] [method "post"])
                 (input ([type "submit"] [value "Next day plox"])))
           (p "Posts:")
           ,@(map (λ (p) (post->section p (all-topics r)  tags)) posts)))]
    [(method (list (regexp yr (list _ y)) (regexp mdr (list _ m d)) rest ...))
     (define dy (maybe-day (string->number y) (string->number m) (string->number d)))
     (if dy
         (day-handler method dy rest bindings)
         not-found)]
    [(#"GET" (list "topics" str))
     (match (get-topic r (string->symbol str))
       [#f not-found]
       [(topic symbol name type)
        (define tagged-posts (get-posts r 'asc (with-tag symbol)))
        (define thread-posts (get-posts r 'asc (in-thread symbol)))
        (define tags (apply tags-hash r (map post-day (append thread-posts tagged-posts))))
        (define tags-html
          (match tags
            ['() '()]
            [_ `((h2 "Posts with the \"" ,(symbol->string symbol) "\"-tag:")
                 (table ,@(apply append (map post->tr tagged-posts))))]))
        (define thread-html
          (match thread-posts
            ['() '()]
            [_ `((h2 "Thread:") ,@(map (λ (p) (post->section-in-thread p tags)) thread-posts))]))
        (ok name
            `(body
              (h1 ,name) ,@tags-html ,@thread-html))])]
    [(#"POST" (list "next-day"))
     (set! the-day (add-days the-day 1))
     (sea-otter "/index.html")]
    [(#"GET" (list "style.css"))
     (response/output (λ (out) (write-string css out)) #:mime-type #"text/css; charset=utf-8")]
    [(method path)
     (printf "not found: ~a, ~a, ~s~n" method path bindings)
     not-found]))

(serve/servlet
 servlet
 #:stateless? #t
 #:servlet-regexp #rx""
 #:servlet-path "/index.html"
 #:extra-files-paths
 (list
  (build-path (current-directory) "static")))


