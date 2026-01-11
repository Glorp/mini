#lang racket/base
(require "day.rkt")
(provide (struct-out post)
         (struct-out before)
         (struct-out after)
         (struct-out in-thread)
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

(struct post (day text thread)
  #:transparent
  #:guard
  (λ (day text thread name)
    (unless
        (and (day? day)
             (valid-text? text)
             (or (not thread) (day? thread)))
      (error 'day "not a valid post: (~a ~s ~s)" name day text))
    (values day text thread)))

(struct before (day) #:transparent)
(struct after (day) #:transparent)
(struct in-thread (day) #:transparent)

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
