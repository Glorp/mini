#lang racket/base
(require "day.rkt")
(provide (struct-out post)
         (struct-out topic)
         (struct-out topics)
         (struct-out before)
         (struct-out after)
         (struct-out in-thread)
         (struct-out with-tag)
         (struct-out tagged)
         valid-text?
         valid-text-split)

(define (valid-text? str)
  (string? str) (<= (string-utf-8-length str) 1024))

(define (utf-8-split str max)
  (define (halp len i)
    (cond [(<= len max) (list (substring str 0 i) (substring str i (string-length str)))]
          [else (define ni (- i 1))
                (halp (- len (string-utf-8-length (substring str ni i))) ni)]))
  (halp (string-utf-8-length str) (string-length str)))

(define (valid-text-split str)
  (utf-8-split str 1024))

(define (valid-id? id)
  (regexp-match #px"^[a-z][a-z0-9-]*[a-z0-9]$" (symbol->string id)))

(struct post (day text id link)
  #:transparent
  #:guard
  (λ (day text id link name)
    (unless
        (and (day? day)
             (valid-text? text)
             (or (not id) (valid-id? id))
             (or (not link) (string? link)))
      (error 'day "not a valid post: (~a ~s ~s ~s ~s)" name day text id link))
    (values day text id link)))

(struct topic (id name)
  #:transparent
  #:guard
  (λ (id name struct-name)
    (unless (and (valid-id? id) (string? name))
      (error 'topic "not a valid topic: (~a ~s ~s)" struct-name id name))
    (values id name)))

(struct topics (list hash)
  #:transparent
  #:guard
  (λ (list hash name)
    (unless (and (list? list) (hash? hash))
      (error 'topic "not a valid topics: (~a ~s ~s)" name list hash))
    (values list hash)))

(struct tagged (day id)
  #:transparent
  #:guard
  (λ (day id name)
    (unless (and (day? day) (valid-id? id))
      (error 'topic "not a valid tagged: (~a ~s ~s)" name day id))
    (values day id)))

(struct before (day) #:transparent)
(struct after (day) #:transparent)
(struct in-thread (id) #:transparent)
(struct with-tag (id) #:transparent)

(module+ test
  (require rackunit)
  (check-equal? (utf-8-split "1234" 4) (list "1234" ""))
  (check-equal? (utf-8-split "1234" 6) (list "1234" ""))
  (check-equal? (utf-8-split "1234" 3) (list "123" "4"))
  (check-equal? (utf-8-split "1234" 2) (list "12" "34"))
  (check-equal? (utf-8-split "1234" 1) (list "1" "234"))
  (check-equal? (utf-8-split "1234" 0) (list "" "1234"))
  (check-equal? (utf-8-split "å234" 4) (list "å23" "4"))
  (check-equal? (utf-8-split "123å" 4) (list "123" "å"))
  (check-equal? (utf-8-split "123å" 2) (list "12" "3å"))
  (check-equal? (utf-8-split "12å4" 4) (list "12å" "4"))
  (check-equal? (utf-8-split "12å4" 3) (list "12" "å4"))
  (check-equal? (utf-8-split "12å4" 2) (list "12" "å4"))
  (check-equal? (utf-8-split "12å4" 1) (list "1" "2å4"))
  (check-equal? (utf-8-split "12å4" 0) (list "" "12å4")))
