#lang racket/base
(require racket/match
         racket/list
         racket/format
         "day.rkt"
         "post.rkt"
         "down.rkt")
(provide page
         day/post->form
         post->section-no-thread
         post->section-in-thread)

(define (post-inputs text)
  `((label "Text: " (textarea ([name "text"]) ,text))
    (label "Optionally a link: " (input ([name "link"])))
    (input ([type "submit"] [value "Submit"]))))

(define (day-url dy)
  (match dy
    [(day y m d) (format "/~a/~a-~a" (pad y 4) (pad m 2) (pad d 2))]))

(define (day->form dy)
  `(form
    ([action ,(~a (day-url dy) "/create")] [method "post"])
    "Make new post:"
    (br)
    ,@(post-inputs "")))

(define (day/post->form dy p)
  (if p
      `(form
        ([action ,(~a (day-url dy) "/edit")] [method "post"])
        "Edit existing post:"
        (br)
        ,@(post-inputs (post-text p)))
      (day->form dy)))

(define (thread-tr tp)
  (match tp
    [(topic symbol name _)
     (define id-str (symbol->string symbol))
     `((tr (th "In thread:")
            (td (a ([href ,(format "/topics/~a" id-str)]) ,name))))]
    [#f '()]))

(define (html-section day text rows)
  (define str (day->string day))
  `(section
    ([id ,str])
    (h2 (a ([href ,(day->link day)]) ,str))
    ,@(parsedown text)
    ,@(if (empty? rows)
          '()
          `((footer (table ,@rows))))))

(define (post->section-in-thread p)
  (match p
    [(post day text _ link) (html-section day text (link-tr link))]))

(define (post->section-no-thread p topics)
  (match p
    [(post day text symbol link)
     (define topic (and symbol (hash-ref (topics-hash topics) symbol)))
     (html-section day text `(,@(thread-tr topic) ,@(link-tr link)))]))

(define (page title body)
  `(html
    ([lang "en"])
    (head
     (meta ([charset "utf-8"]))
     (title ,title)
     (link ([href "/style.css"] [rel "stylesheet"])))
    ,body))

(define (pad n w)
  (~a n #:min-width w #:align 'right #:pad-string "0"))
(define (day->link dy)
  (match dy
    [(day y m d) (format "/~a/~a/~a" (pad y 4) (pad m 2) (pad d 2))]))

(define (link-tr link)
  (if link
      `((tr (th "Links to:") (td (a ([href ,link]) ,link))))
      '()))
