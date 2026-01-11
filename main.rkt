#lang racket/base

(require racket/match
         racket/format
         web-server/servlet
         web-server/servlet-env
         "day.rkt"
         "post.rkt"
         "repo.rkt"
         "html.rkt"
         "down.rkt")

(define the-day (today))
(define r (open-repo 'memory))
(create-post r (post (add-days the-day -5) "blah bla\nbla\n\nblep" #f))
(create-post r (post (add-days the-day -3) "beep\nboop\nbap" #f))
(create-post r (post (add-days the-day -1) "okay" #f))

(define (out-html htmlx)
  (λ (out)
    (write-string doctype out)
    (write-html htmlx out)))

(define (ok title body)
  (response/output (out-html (page title body))))

(define (page title body)
  `(html
    ([lang "en"])
    (head (meta ([charset "utf-8"])) (title "welp"))
    ,body))

(define (not-found)
  (define htmlx (page "404 Not Found :(" '(body (h1 "Not found :("))))
  (response 404 #"Not Found" (current-seconds) TEXT/HTML-MIME-TYPE '() (out-html htmlx)))

(define (sea-otter path)
  (define headers (list (header #"location" (string->bytes/utf-8 path))))
  (response 303 #"Sea Otter" (current-seconds) #f headers (λ (out) (void))))

(define (bad str . args)
  (define htmlx (page "400 Bad Request :(" `(body (h1 "Bad Request :(") (p ,(apply format str args)))))
  (response 400 #"Not Found" (current-seconds) TEXT/HTML-MIME-TYPE '() (out-html htmlx)))

(define index '(a ([href "/index.html"]) "back to index"))

(define mdr #px"^(\\d\\d)-(\\d\\d)$")
(define yr #px"^(\\d\\d\\d\\d)$")

(define (post->html-list p)
  (match p
    [(post day text _)
     `((h2 ,(day->string day))
       ,@(parsedown text))]))

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
     (ok "blep"
         `(body
           (h1 "blah blah")
           (p "today really is " ,(day->string the-day))
           (form ([action "next-day"] [method "post"])
                 (input ([type "submit"] [value "Next day plox"])))
           (form
            ([action ,(format "~a/~a-~a/post" (pad y 4) (pad m 2) (pad d 2))] [method "post"])
            (label "Text: " (textarea ([name "text"])))
            (input ([type "submit"] [value "Submit"])))
           (p "posts:")
           ,@(apply append (map post->html-list posts))))]
    [(#"POST" (list (regexp yr (list _ y)) (regexp mdr (list _ m d)) "post"))
     (match bindings
       [`((text . ,crlftext))
        (define text (lfstring crlftext))
        (define dy (maybe-day (string->number y) (string->number m) (string->number d)))
        (cond [(not dy) (not-found)]
              [(not (valid-text? text)) (bad ">:( text too long: ~s" text)]
              [(get-post r dy) (bad "there's already a post for ~a" (day->string dy))]
              [else (create-post r (post dy text #f))
                    (ok "success :)"
                        `(body
                          (h1 "nice :)")
                          (p "created post for " ,(day->string dy))
                          (p ,index)))])]
       [_ (bad "~s <- what is ??" bindings)])]
    [(#"POST" (list "next-day"))
     (set! the-day (add-days the-day 1))
     (sea-otter "/index.html")]
    [(method path)
     (printf "not found: ~a, ~a, ~s~n" method path bindings)
     (not-found)]))

(serve/servlet
 servlet
 #:servlet-regexp #rx""
 #:servlet-path "/index.html")


