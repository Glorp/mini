#lang racket/base
(require racket/match
         racket/list
         racket/set
         racket/format
         "day.rkt"
         "post.rkt"
         "down.rkt")
(provide page
         day->url
         day/post->form
         post->section
         post->section-in-thread
         post->tr
         tag-forms)

(define (post-inputs text link)
  `((label "Text: " (textarea ([name "text"]) ,text))
    (label "Optionally a link: " (input ([name "link"] [type "text"] [value ,(or link "")])))
    (input ([type "submit"] [value "Submit"]))))

(define (day->url dy)
  (match dy
    [(day y m d) (format "/~a/~a-~a" (4pad y) (2pad m) (2pad d))]))

(define (day->form dy)
  `(form
    ([action ,(~a (day->url dy) "/create")] [method "post"])
    "Make new post:"
    (br)
    ,@(post-inputs "" "")))

(define (day/post->form dy p)
  (if p
      `(form
        ([action ,(~a (day->url dy) "/update")] [method "post"])
        (p "Edit existing post:")
        ,@(post-inputs (post-text p) (post-link p)))
      (day->form dy)))

(define (thread-link tp)
  (match tp
    [(topic symbol name _)
     (define id-str (symbol->string symbol))
     `(a ([href ,(format "/topics/~a" id-str)]) ,name)]))

(define (tag-link tp)
  (match tp
    [(topic symbol name _)
     (define id-str (symbol->string symbol))
     `(a ([href ,(format "/topics/~a" id-str)]) ,(symbol->string symbol))]))

(define (post->tr p)
  (match p
    [(post dy text _ _)
     (define start (match (regexp-match #px"[^\n]+" text)
                     [#f ""]
                     [(list s) s]))
     (define str (if (> (string-length start) 64)
                     (substring start 0 61)
                     start))
     `((tr (th (a ([href ,(day->url dy)]) (time ,(day->string dy))))
           (td ,@(parseline str))))]))

(define (tags-tr tps)
  (match tps
    ['() '()]
    [(list tp rest ...)
     `((tr (th "Tagged:")
           (td ,(tag-link tp) ,@(apply append (map (λ (t) `(", " ,(tag-link t))) rest)))))]))

(define (html-section id h text rows)
  `(section
    ([id ,id])
    ,h
    ,@(parsedown text)
    ,@(if (empty? rows)
          '()
          `((footer (table ,@rows))))))

(define (post->section-in-thread p tags)
  (match p
    [(post day text _ link)
     (html-section (day->string day)
                   `(h3 (time ,(day->string day)))
                   text
                   `(,@(link-tr link) ,@(tags-tr (hash-ref tags day '()))))]))

(define (post->section p topics tags)
  (match p
    [(post day text symbol link)
     (define topic (and symbol (hash-ref (topics-hash topics) symbol)))
     (html-section (day->string day)
                   `(h3 (a ([href ,(day->url day)])
                           (time ,(day->string day))
                           ,@(if topic
                                 `(" (" ,(topic-name topic) , ")")
                                 '())))
                   text
                   `(,@(link-tr link) ,@(tags-tr (hash-ref tags day '()))))]))

(define (tag-forms dy topics tags)
  (define tagset (apply set (map topic-symbol (hash-ref tags dy '()))))
  (define (halp l adds removes)
    (match l
      ['() (list (reverse adds) (reverse removes))]
      [(list (topic symbol _ _) rest ...) (if (set-member? tagset symbol)
                                              (halp rest adds (cons symbol removes))
                                              (halp rest (cons symbol adds) removes))]))
  (match (halp (topics-tags topics) '() '())
    [(list adds removes)
     (define url (day->url dy))
     `((form ([method "post"] [action ,(~a url "/tag")])
             (p "Add a tag:")
             ,@(map (λ (tag)
                      `(input ([type "submit"]
                               [name ,(symbol->string tag)]
                               [value ,(symbol->string tag)])))
                    adds))
       (form ([method "post"] [action ,(~a url "/untag")])
             (p "Remove a tag:")
             ,@(map (λ (tag)
                      `(input ([type "submit"]
                               [name ,(symbol->string tag)]
                               [value ,(symbol->string tag)])))
                    removes)))]))

(define (page title body)
  `(html
    ([lang "en"])
    (head
     (meta ([charset "utf-8"]))
     (title ,title)
     (link ([href "/style.css"] [rel "stylesheet"])))
    ,body))

(define (link-tr link)
  (if link
      `((tr (th "Links to:") (td (a ([href ,link]) ,link))))
      '()))
