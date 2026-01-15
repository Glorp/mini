#lang racket/base
(require racket/match
         racket/format)

(provide (struct-out day)
         maybe-day
         seconds->day
         today
         day->string
         maybe-day->string
         string->day
         string->maybe-day
         add-days
         2pad
         4pad)

(define (2pad n)
  (~a n #:width 2 #:align 'right #:pad-string "0"))
(define (4pad n)
  (~a n #:width 4 #:align 'right #:pad-string "0"))

(define (leap-year? y)
  (and (zero? (remainder y 4))
       (or (not (zero? (remainder y 100)))
           (zero? (remainder y 400)))))

(define days-per-month
  (vector 31 31 28 31 30 31 30 31 31 30 31 30 31))

(define (days-in-month y m)
  (if (and (= m 2) (leap-year? y))
      29
      (vector-ref days-per-month m)))

(define (normalize y m d)
  (cond [(> m 12) (normalize (+ y 1) (- m 12) d)]
        [(< m 1) (normalize (- y 1) (+ m 12) d)]
        [else
         (define days (days-in-month y m))
         (cond
           [(> d days) (normalize y (+ m 1) (- d days))]
           [(< d 1) (normalize y (- m 1) (+ d (days-in-month y (- m 1))))]
           [else (day y m d)])]))

(define (add-days dy i)
  (match dy
    [(day y m d)
     (normalize y m (+ d i))]))

(define (next-day dy)
  (add-days dy 1))

(define (previous-day dy)
  (add-days dy -1))


(define (seconds->day s)
  (define d (seconds->date s #f))
  (day (date-year d) (date-month d) (date-day d)))
(define (today)
  (seconds->day (current-seconds)))

(define (valid? y m d)
  (and (exact-positive-integer? y)
       (and (exact-positive-integer? m) (> m 0) (<= m 12))
       (and (exact-positive-integer? d) (> d 0) (<= d (days-in-month y m)))))

(struct day (y m d)
  #:transparent
  #:guard
  (λ (y m d name)
    (unless (valid? y m d)
      (error 'day "not a valid day: (~a ~s ~s ~s)" name y m d))
    (values y m d)))

(define (maybe-day y m d)
  (and (valid? y m d) (day y m d)))

(define (day->string dy)
  (match dy
    [(day y m d) (format "~a-~a-~a" (4pad y) (2pad m) (2pad d))]))

(define (maybe-day->string d)
  (and (day? d) (day->string d)))

(define (string->maybe-day str)
  (match (regexp-match #px"^(\\d+)-(\\d+)-(\\d+)$" str)
    [(list _ ys ms ds)
     (define y (string->number ys))
     (define m (string->number ms))
     (define d (string->number ds))
     (and (valid? y m d)
          (day y m d))]
    [_ #f]))

(define (string->day str)
  (or (string->maybe-day str)
      (error 'string->day "bad argument: ~a" str)))

(module+ test
  (require rackunit)
  (define d (today))
  (define s (day->string d))
  (define sd (string->day s))
  (check-equal? sd d)
  (define d2 (next-day d))
  (check-not-equal? d2 d)
  (define (check-days a b i)
    (check-equal? (add-days a i) b)
    (check-equal? (add-days b (- i)) a))
  (check-equal? (string->day "2025-06-25") (day 2025 6 25))
  (check-days (day 2025 6 25) (day 2025 6 26) 1)
  (check-days (day 2025 6 30) (day 2025 7 1) 1)
  (check-days (day 2025 12 31) (day 2026 1 1) 1)
  (check-days (day 2025 12 31) (day 2026 1 1) 1)
  (check-days (day 2016 2 28) (day 2016 2 29) 1)
  (check-days (day 2016 2 29) (day 2016 3 1) 1)
  (check-days (day 2017 2 28) (day 2017 3 1) 1)
  (check-days (day 2017 2 28) (day 2017 3 1) 1)
  (check-days (day 2017 2 20) (day 2017 3 1) 9)
  (check-days (day 2017 2 20) (day 2017 3 20) 28)
  (check-days (day 2017 2 20) (day 2018 2 20) 365)
  (check-days (day 2020 2 20) (day 2021 2 20) 366))
