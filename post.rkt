#lang racket/base
(require "day.rkt")
(provide (struct-out post)
         (struct-out before)
         (struct-out after)
         (struct-out in-thread))

(struct post (day text thread)
  #:transparent
  #:guard
  (λ (day text thread name)
    (unless
        (and (day? day)
             (and (string? text) (<= (string-utf-8-length text) 1024))
             (or (not thread) (day? thread)))
      (error 'day "not a valid post: (~a ~s ~s)" name day text))
    (values day text thread)))

(struct before (day) #:transparent)
(struct after (day) #:transparent)
(struct in-thread (day) #:transparent)
