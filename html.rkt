#lang racket/base
(require (only-in racket/match match match-define)
         (only-in racket/set set set-member?)
         (only-in racket/format ~a)
         "day.rkt"
         "post.rkt"
         "down.rkt")
(provide page
         day->url
         symbol->url
         topic->url
         topic-content
         new-topic-form
         topic->forms
         day/post->forms
         post->section
         post->section-in-thread
         post->tr
         topics->table
         tag-forms)

(define (post-inputs text symbol link topics)
  (define sym (or (and symbol (symbol->string symbol)) ""))
  `((label "Text: " (textarea ([name "text"]) ,text))
    " "
    (label "Thread: " (input ([name "symbol"] [type "text"] [value ,sym] [list "topics"])))
    " "
    (datalist ([id "topics"])
              ,@(map (λ (tp)
                       (match tp
                         [(topic symbol name _)
                          `(option ([value ,(symbol->string symbol)] [label ,name]))]))
                     (topics-threads topics)))
    " "
    (label "Optionally a link: " (input ([name "link"] [type "text"] [value ,(or link "")])))
    " "
    (input ([type "submit"] [value "Submit"]))))

(define (day->url dy rest)
  (match dy
    [(day y m d) (format "/~a/~a-~a~a" (4pad y) (2pad m) (2pad d) rest)]))


(define (topic-inputs name type)
  (define (selected sym)
    (if (equal? type sym)
        '([selected "selected"])
        '()))
  `((label "Name: " (input ([type "text"] [name "name"] [value ,name])))
    " "
    (label "Suggested as thread/tag: "
           (select ([name "type"])
                   (option ([value "neither"] ,@(selected 'either)) "Neither")
                   (option ([value "thread"] ,@(selected 'thread)) "Thread")
                   (option ([value "tag"] ,@(selected 'tag)) "Tag")
                   (option ([value "either"] ,@(selected 'either)) "Either")))
    " "
    (input ([type "submit"]))))

(define new-topic-form
  `(form ([method "post"] [action "/topics/create"])
         (label "Symbol: " (input ([type "text"] [name "symbol"])))
         ,@(topic-inputs "" 'neither)))

(define (topic->forms tp)
  (match tp
    [(topic sym name type)
     `((p "Edit topic:")
       (form ([method "post"] [action ,(symbol->url sym "/update")])
             ,@(topic-inputs name type))
       (p "Delete topic:")
       (form ([method "post"] [action ,(symbol->url sym "/delete")])
             (input ([type "submit"] [name "submit"] [value "Delete"]))))]))

(define (symbol->url sym . rest)
  (format "/topic/~a~a" (symbol->string sym) (apply ~a rest)))

(define (topic->url tp . rest)
  (match tp
    [(topic sym _ _) (apply symbol->url sym rest)]))

(define (day->form dy topics)
  `(form
    ([action ,(day->url dy "/create")] [method "post"])
    ,@(post-inputs "" #f #f topics)))

(define (day/post->forms dy p topics)
  (match p
    [#f `((p "Make new post:") ,(day->form dy topics))]
    [(post dy t sym link)
     `((p "Edit post:")
       (form ([action ,(day->url dy "/update")] [method "post"]) ,@(post-inputs t sym link topics))
       (p "Delete post:")
       (form ([action ,(day->url dy "/delete")] [method "post"])
             (input ([type "submit"] [name "submit"] [value "Delete"]))))]))

(define (thread-link tp)
  (match tp
    [(topic sym name _)
     (define id-str (symbol->string sym))
     `(a ([href ,(format "/topics/~a" id-str)]) ,name)]))

(define (tag-link tp)
  (match tp
    [(topic sym name _)
     (define id-str (symbol->string sym))
     `(a ([href ,(symbol->url sym ".html")]) ,(symbol->string sym))]))

(define (post->tr p)
  (match p
    [(post dy text _ _)
     (define start (match (regexp-match #px"[^\n]+" text)
                     [#f ""]
                     [(list s) s]))
     (define str (if (> (string-length start) 64)
                     (~a (substring start 0 61) "...")
                     start))
     `((tr (th (a ([href ,(day->url dy ".html")]) (time ,(day->string dy))))
           (td ,@(parseline str))))]))

(define (tags-tr tps)
  (match tps
    ['() '()]
    [(list tp rest ...)
     `((tr (th "Tagged:")
           (td ,(tag-link tp) ,@(apply append (map (λ (t) `(", " ,(tag-link t))) rest)))))]))

(define (edit-post-tr user dy)
  (if user
      `((tr (th ([colspan "2"]) (a ([href ,(day->url dy "/edit")]) "Edit post"))))
      '()))

(define (html-section user h p tags)
  (match p
    [(post dy text _ link)
     (define rows `(,@(link-tr link)
                    ,@(tags-tr (hash-ref tags dy '()))
                    ,@(edit-post-tr user dy)))
     `(section
       ([id ,(day->string dy)])
       ,h
       ,@(parsedown text)
       ,@(match rows
           ['() '()]
           [_ `((footer (table ,@rows)))]))]))  

(define (post->section-in-thread user p tags)
  (html-section user `(h3 (time ,(day->string (post-day p)))) p tags))

(define (post->section user p topics tags)
  (match p
    [(post dy _ sym _)
     (define topic (and sym (hash-ref (topics-hash topics) sym)))
     (define h `(h3 (a ([href ,(day->url dy ".html")])
                       (time ,(day->string dy))
                       ,@(if topic
                             `(" (" ,(topic-name topic) , ")")
                             '()))))
     (html-section user h p tags)]))

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
     `(,@(tag-form "Add a tag:" (day->url dy "/tag") adds)
       ,@(tag-form "Remove a tag:" (day->url dy "/untag") removes))]))

(define (tag-form title action tags)
  (match tags
    ['() '()]
    [_ `((p ,title)
         (form ([class "tags"] [method "post"] [action ,action])
               ,@(map (λ (tag)
                        `(input ([type "submit"]
                                 [name ,(symbol->string tag)]
                                 [value ,(symbol->string tag)])))
                      tags)))]))

(define (header user)
  `(header (nav (ol (li (a ([href "/index.html"]) "Index"))
                    " "
                    (li (a ([href "/topics.html"]) "Topics"))
                    " "
                    (li (a ([href "/archive.html"]) "Archive"))))))

(define (page user title content)
  (define loggedin
    (if user
        `((p "You are logged in. Hi, " ,user " :)"))
        '()))
  `(html
    ([lang "en"])
    (head
     (meta ([charset "utf-8"]))
     (title ,title)
     (link ([href "/style.css"] [rel "stylesheet"])))
    (body ,(header user) ,@loggedin ,@content)))

(define (link-tr link)
  (if link
      `((tr (th "Links to:") (td (a ([href ,link]) ,link))))
      '()))

(define (topics->table tps)
  (match tps
    [(topics _ all _ _)
     `(table
       (thead (tr (th "Topic") (th "Symbol") (th "Thread/tag")))
       ,@(map topic-tr all))]))

(define (topic-tr tp)
  (match tp
    [(topic sym name type)
     `(tr (td (a ([href ,(symbol->url sym ".html")]) ,name))
          (td ,(symbol->string sym))
          (td ,(symbol->string type)))]))

(define (topic-content user tp thread-posts tagged-posts tags)
  (match tp
    [(topic sym name type)
     (define tags-html
       (match tagged-posts
         ['() '()]
         [_ `((h2 "Posts with the \"" ,(symbol->string sym) "\"-tag:")
              (table ,@(apply append (map post->tr tagged-posts))))]))
     (match-define (list thread-first thread-rest)
       (match thread-posts
         ['() '(() ())]
         [(list first rest ...)
          `((,(post->section-in-thread user first tags))
            (,@(map (λ (p) (post->section-in-thread user p tags)) rest)))]))
     (define edit
       (if user
           (topic->forms tp)
           '()))
     `((h1 ,name) ,@edit ,@thread-first ,@tags-html ,@thread-rest)]))
