#lang racket/base

(require racket/match
         web-server/servlet
         web-server/servlet-env
         "repo.rkt"
         "html.rkt"
         "day.rkt"
         "down.rkt")

(define r (open-repo 'memory))


(define (ok htmlx)
  (response
   200
   #"OK"
   (current-seconds)
   TEXT/HTML-MIME-TYPE
   '()
   (λ (out)
     (write-string doctype out)
     (write-html htmlx out))))

(define (page title body)
  `(html
    ([lang "en"])
    (head (meta ([charset "utf-8"])) (title "welp"))
    ,body))

(define (not-found)
  (response
   404
   #"Not Found"
   (current-seconds)
   TEXT/HTML-MIME-TYPE
   '()
   (λ (out)
     (write-string doctype out)
     (write-html (page "404 Not Found :(" '(body (h1 "Not found :("))) out))))

(define (servlet req)
  (define path (map path/param-path (url-path (request-uri req))))
  (match* ((request-method req) path (request-bindings req))
    [(#"GET" (or '("") '("index.html")) _)
     (ok (page "Blep"
               '(body
                 (h1 "blah blah")
                 (p "click: "
                    (a ([href "/foo/bar?blep=ohno&blap=okay&blap=ohno"]) "link"))
                 (form
                  ([action "/bar/foo"] [method "post"])
                  (label "Text: " (textarea ([name "text"]) "beep"))
                  (label "Text: " (textarea ([name "text"]) "boop"))
                  (label "Stuff: " (input ([name "stuff"] [value "bap"])))
                  (input ([type "submit"] [value "Submit"]))))))]
    [(#"GET" '("foo" "bar") `((blep . ,blep) (blap . , blap1) (blap .,blap2)))
     (define thank (format "got: ~a, ~a, ~a" blep blap1 blap2))
     (displayln thank)
     (ok (page "thank" `(body (h1 "thank") (p ,thank))))]
    [(#"POST" '("bar" "foo") `((text . ,text1) (text . , text2) (stuff .,stuff)))
     (define thank (format "got: ~a, ~a, ~a" text1 text2 stuff))
     (displayln thank)
     (ok (page "thank" `(body (h1 "thank") (p ,thank))))]
    [(a b c)
     (printf "derp: ~s, ~s, ~s~n" a b c)
     (not-found)]))

(serve/servlet
 servlet
 #:servlet-regexp #rx""
 #:servlet-path "")


