#lang racket/base

(require "repo.rkt"
         "html.rkt"
         "day.rkt"
         web-server/servlet
         web-server/servlet-env)

(define (servlet req)
  (printf "~a~n~n~n" req)
  (response
   200
   #"OK"
   (current-seconds)
   TEXT/HTML-MIME-TYPE
   '()
   (λ (out)
     (write-string doctype out)
     (write-html
      '(html
        ([lang "en"])
        (head (meta ([charset "utf-8"])) (title "welp"))
        (body (h1 "hello") (p "hi")))
      out))))
  
(serve/servlet
 servlet
 #:servlet-regexp #rx"")

