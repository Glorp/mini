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
         symbol->url
         topic->url
         day/post->form
         post->section
         post->section-in-thread
         post->tr
         tag-forms)

(define (post-inputs text symbol link topics)
  (define sym (or (and symbol (symbol->string symbol)) ""))
  `((label "Text: " (textarea ([name "text"]) ,text))
    (label "Thread: " (input ([name "symbol"] [type "text"] [value ,sym] [list "topics"])))
    (datalist ([id "topics"])
              ,@(map (λ (tp)
                       (match tp
                         [(topic symbol name _)
                          `(option ([value ,(symbol->string symbol)] [label ,name]))]))
                     (topics-threads topics)))
    (label "Optionally a link: " (input ([name "link"] [type "text"] [value ,(or link "")])))
    (input ([type "submit"] [value "Submit"]))))

(define (day->url dy rest)
  (match dy
    [(day y m d) (format "/~a/~a-~a~a" (4pad y) (2pad m) (2pad d) rest)]))

(define (symbol->url sym . rest)
  (format "/topic/~a~a" (symbol->string sym) (apply ~a rest)))

(define (topic->url tp . rest)
  (match tp
    [(topic sym _ _) (apply symbol->url sym rest)]))

(define (day->form dy topics)
  `(form
    ([action ,(day->url dy "/create")] [method "post"])
    "Make new post:"
    (br)
    ,@(post-inputs "" #f #f topics)))

(define (day/post->form dy p topics)
  (match p
    [#f (day->form dy topics)]
    [(post dy t sym link)
     `(form
       ([action ,(day->url dy "/update")] [method "post"])
       (p "Edit existing post:")
       ,@(post-inputs t sym link topics))]))

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
     `((tr (th (a ([href ,(day->url dy ".html")]) (time ,(day->string dy))))
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

(define (edit-post-tr user dy)
  (if user
      `((tr (th (a ([colspan "2"] [href ,(day->url dy "/edit")]) "Edit post"))))
      '()))

(define (post->section-in-thread user p tags)
  (match p
    [(post dy text _ link)
     (html-section (day->string dy)
                   `(h3 (time ,(day->string dy)))
                   text
                   `(,@(link-tr link)
                     ,@(tags-tr (hash-ref tags dy '()))
                     ,@(edit-post-tr user dy)))]))

(define (post->section user p topics tags)
  (match p
    [(post dy text symbol link)
     (define topic (and symbol (hash-ref (topics-hash topics) symbol)))
     (html-section (day->string dy)
                   `(h3 (a ([href ,(day->url dy ".html")])
                           (time ,(day->string dy))
                           ,@(if topic
                                 `(" (" ,(topic-name topic) , ")")
                                 '())))
                   text
                   `(,@(link-tr link)
                     ,@(tags-tr (hash-ref tags dy '()))
                     ,@(edit-post-tr user dy)))]))

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
     `((form ([method "post"] [action ,(day->url dy "/tag")])
             (p "Add a tag:")
             ,@(map (λ (tag)
                      `(input ([type "submit"]
                               [name ,(symbol->string tag)]
                               [value ,(symbol->string tag)])))
                    adds))
       (form ([method "post"] [action ,(day->url dy "/untag")])
             (p "Remove a tag:")
             ,@(map (λ (tag)
                      `(input ([type "submit"]
                               [name ,(symbol->string tag)]
                               [value ,(symbol->string tag)])))
                    removes)))]))

(define nav '(nav (a ([href "/index.html"]) "Index")
                  ", " (a ([href "/topics.html"]) "Topics")))

(define (header user)
  `(header ,nav
           ,@(if user
                 `((p "You are logged in. Hi, " ,user " :)"))
                 '())))

(define (page user title content)
  `(html
    ([lang "en"])
    (head
     (meta ([charset "utf-8"]))
     (title ,title)
     (link ([href "/style.css"] [rel "stylesheet"])))
    (body ,(header user) ,@content)))

(define (link-tr link)
  (if link
      `((tr (th "Links to:") (td (a ([href ,link]) ,link))))
      '()))
