#lang racket/base
(require racket/match
         racket/format)

(provide (struct-out day)
         seconds->day
         today
         day->string
         string->day
         add-days)

(define (leap-year? y)
  (and (zero? (remainder y 4))
       (or (not (zero? (remainder y 100)))
           (zero? (remainder y 400)))))

(define days-per-month
  (vector 31 28 31 30 31 30 31 31 30 31 30 31))

(define (days-in-month y m)
  (if (and (= m 2) (leap-year? y))
      29
      (vector-ref days-per-month (- m 1))))

(define (add-days dy i)
  (match dy
    [(day y m d)
     (define days (days-in-month y m))
     (define nd (+ d i))
     (cond [(> nd days) (add-days (if (>= m 12)
                                      (day (+ y 1) 1 1)
                                      (day y (+ m 1) 1))
                                  (- nd days 1))]
           [(< nd 1) (add-days (if (<= m 1)
                                   (day (- y 1) 12 31)
                                   (day y (- m 1) (days-in-month y (- m 1))))
                               nd)]
           [else (day y m nd)])]))

(define (next-day dy)
  (add-days dy 1))

(define (previous-day dy)
  (add-days dy -1))


(define (seconds->day s)
  (define d (seconds->date s #f))
  (day (date-year d) (date-month d) (date-day d)))
(define (today)
  (seconds->day (current-seconds)))
  
(struct day (y m d) #:transparent)

(define (day->string d)
  (define (pad v n)
    (~a v #:width n #:align 'right #:left-pad-string "0"))
  (format "~a-~a-~a"
          (pad (day-y d) 4)
          (pad (day-m d) 2)
          (pad (day-d d) 2)))

(define (string->day str)
  (match (regexp-match #px"^(\\d+)-(\\d+)-(\\d+)$" str)
    [(list _ y m d) (day (string->number y) (string->number m) (string->number d))]
    [_ (error 'string->day "bad argument: ~a" str)]))

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
