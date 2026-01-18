#lang racket/base

(require racket/match
         racket/string
         racket/format
         racket/file
         web-server/servlet
         "day.rkt"
         "post.rkt"
         "repo.rkt"
         "html.rkt"
         "down.rkt"
         "site.rkt")

(provide servlet)

(define css (file->string "./style.css"))

(define (out-html html)
  (λ (out)
    (write-html html out)))

(define (ok user title content)
  (response/output (out-html (page user title content))))

(define (not-found user)
  (let ([htmlx (page user "404 Not Found :(" '((h1 "Not found :(")))])
    (response 404 #"Not Found" (current-seconds) TEXT/HTML-MIME-TYPE '() (out-html htmlx))))

(define (sea-otter path)
  (define headers (list (header #"location" (string->bytes/utf-8 path))))
  (response 303 #"Sea Otter" (current-seconds) #f headers (λ (out) (void))))

(define (bad user content)
  (define htmlx (page user "400 Bad Request :(" content))
  (response 400 #"Not Found" (current-seconds) TEXT/HTML-MIME-TYPE '() (out-html htmlx)))

(define index '(a ([href "/index.html"]) "back to index"))

(define mdr #px"^(\\d\\d)-(\\d\\d)$")
(define mdr-dot-html #px"^(\\d\\d)-(\\d\\d)[.]html$")
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



(define (maybe-login req login?)
  (match (request->basic-credentials req)
    [#f #f]
    [(cons userbytes pwdbytes)
     (and (login? userbytes pwdbytes) (bytes->string/utf-8 userbytes))]))

(define (servlet r login? get-today)

  (define (store-post user dy bindings store)
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

  (define (day-handler user method dy rest bindings)
    (define dstr (day->string dy))
    (define p (and dy (get-post r dy)))
    (match* (method rest)
      [(#"GET" '())
       (define sym (and p (post-symbol p)))
       (cond [sym (sea-otter (symbol->url sym ".html#" dstr))]
             [p (ok user (day->string dy) `(,(post->section-in-thread user p (tags-hash r dy))))]
             [else (not-found user)])]
      [(#"GET" '("edit"))
       (define topics (all-topics r))
       (define tags (tags-hash r dy))
       (ok user
           dstr
           `((h1 ,dstr)
             ,@(cond [p `((p "Existing post:")
                          ,(post->section user p topics tags)
                          ,@(tag-forms dy topics tags))]
                     [else `((p "There's no existing post for " (time ,dstr) "."))])
             ,@(day/post->forms dy p topics)))]
      [(#"POST" '("create"))
       (if (get-post r dy)
           (bad user `((h1 "there's already a post for " (time ,dstr))))
           (store-post user dy bindings create-post))]
      [(#"POST" '("update"))
       (if (get-post r dy)
           (store-post user dy bindings update-post)
           (bad user `((h1 "there's no post for " ,dstr " to edit. hmm!"))))]
      [(#"POST" '("update"))
       (if (get-post r dy)
           (store-post user dy bindings update-post)
           (bad user `((h1 "there's no post for " ,dstr " to edit. hmm!"))))]
      [(#"POST" '("delete"))
       (match (get-post r dy)
         [#f (bad user `((h1 "there's no post for " ,dstr " to edit. hmm!")))]
         [p (delete-post r p)
            (sea-otter (day->url dy "/edit"))])]
      [(#"POST" '("tag")) (tag-handler user dy bindings tag)]
      [(#"POST" '("untag")) (tag-handler user dy bindings untag)]
      [(method path)
       (printf "not found: ~a ~a, ~a, ~s~n" dy method path bindings)
       (not-found user)]))

  (define (tag-handler user dy bindings tag/untag)
    (match (get-post r dy)
      [#f (bad user `((h1 "there's no post for " ,(day->string dy) " to tag. hmm!")))]
      [p (match bindings
           [(list (cons sym _))
            (match (hash-ref (topics-hash (all-topics r)) sym #f)
              [#f (bad user `((h1 "bad params. hmm!")))]
              [tp (tag/untag r p tp)
                  (sea-otter (day->url dy "/edit"))])]
           [_ (bad user `((h1 "bad params. hmm!")))])]))

  (λ (req)
    (define the-day (get-today))
    (define user (maybe-login req login?))
    (define method (request-method req))
    (match* (method user)
      [(#"POST" #f) unauthorized]
      [(_ _)
       (define bindings (request-bindings req))
       (match* (method (map path/param-path (url-path (request-uri req))))
         [(#"GET" '("login")) (if user (sea-otter "/index.html") unauthorized)]
         [(#"GET" (or '("") '("index.html")))
          (define posts (get-posts r 'desc (before the-day)))
          (define tags (apply tags-hash r (map post-day posts)))
          (ok user
              "Miniature weblog"
              `((h1 "Miniature weblog")
                ,@(if user
                      `((p "Today really is: "
                           (a ([href ,(day->url the-day "/edit")]),(day->string the-day))
                           "."))
                      '())
                ,@(map (λ (p) (post->section user p (all-topics r)  tags)) posts)))]
         [(method (list (regexp yr (list _ y)) (regexp mdr (list _ m d)) rest ...))
          (define dy (maybe-day (string->number y) (string->number m) (string->number d)))
          (if dy
              (day-handler user method dy rest bindings)
              (not-found user))]
         [(method (list (regexp yr (list _ y)) (regexp mdr-dot-html (list _ m d))))
          (define dy (maybe-day (string->number y) (string->number m) (string->number d)))
          (if dy
              (day-handler user method dy '() bindings)
              (not-found user))]
         [(#"GET" (list (or "topics" "topics.html")))
          (define new-topic
            (if user
                `((p "Create new topic:") ,new-topic-form)
                '()))
          (ok user
              "Topics"
              `((h1 "Topics")
                ,@new-topic
                ,@(map (λ (tp)
                         (match tp
                           [(topic sym name type)
                            `(p (a ([href ,(symbol->url sym ".html")]) ,name))]))
                       (topics-all (all-topics r)))))]
         [(#"GET" (list "topic" (regexp #px"^(.+?)[.]html$" (list _ str))))
          (define tp (get-topic r (string->symbol str)))
          (match tp
            [#f (not-found user)]
            [(topic symbol name type)
             (define tagged-posts (get-posts r 'asc (with-tag symbol)))
             (define thread-posts (get-posts r 'asc (in-thread symbol)))
             (define tags (apply tags-hash r (map post-day (append thread-posts tagged-posts))))
             (define tags-html
               (match tagged-posts
                 ['() '()]
                 [_ `((h2 "Posts with the \"" ,(symbol->string symbol) "\"-tag:")
                      (table ,@(apply append (map post->tr tagged-posts))))]))
             (match-define (list thread-first thread-rest)
               (match thread-posts
                 ['() '(() ())]
                 [(list first rest ...)
                  `((,(post->section-in-thread user first tags))
                    (,@(map (λ (p) (post->section-in-thread user p tags)) rest)))]))
             (define edit
               (if user
                   (topic->forms tp)
                   '()))
             (ok user name `((h1 ,name) ,@edit ,@thread-first ,@tags-html ,@thread-rest))])]
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
               (sea-otter "/topics")])]
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
