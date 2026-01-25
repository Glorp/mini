#lang racket/base
(require (only-in racket/match match match*)
         (only-in racket/format ~a)
         "day.rkt"
         "post.rkt"
         "repo.rkt"
         "html.rkt")
(provide index
         archive-page
         month-page
         topics-page
         topic-page
         day-page
         edit-post-page)

(define (index r user the-day)
  (define posts (get-posts r 'desc (before the-day)))
  (define tags (apply tags-hash r (map post-day posts)))
  (page user
        "Miniature weblog"
        `((h1 "Miniature weblog")
          ,@(if user
                `((p "Today really is: "
                     (a ([href ,(day->url the-day "/edit")])
                        ,(day->string the-day))
                     "."))
                '())
          ,@(map (λ (p) (post->section user p (all-topics r)  tags)) posts))))

(define (archive-page r user the-day)
  (define title "Archive")
  (match* (the-day (get-posts r 'asc #:limit 1))
    [(_ '()) (page user title `((h1 ,title) (p "There are no posts.")))]
    [((day to-y to-m _) (list (post (day from-y from-m _) _ _ _)))
     (define res
       (apply
        append
        (for/list ([y (in-range from-y (+ to-y 1))])
          (define months
            (for/list ([m (in-range 1 13)]
                       #:unless (or (and (= y from-y) (< m from-m))
                                    (and (= y to-y) (> m to-m))))
              `(", "
                (a ([href ,(format "/~a/~a.html" (4pad y) (2pad m))])
                   ,(format "~a" (2pad m))))))
          `((h2 ,(4pad y)) ,@(cdr (apply append months))))))
     
     (page user title `((h1 ,title) ,@res))]))

(define (month-page r user y m)
  (define dy (day y m 1))
  (define prev (add-days dy -1))
  (define next (normalized-day y (+ m 1) 1))
  (define posts (get-posts r 'asc (after prev) (before next)))
  (define tags (apply tags-hash r (map post-day posts)))
  (define title (format "Archive:  ~a-~a" (4pad y) (2pad m)))
  (page user
        title
        `((h1 ,title)
          ,@(map (λ (p) (post->section user p (all-topics r)  tags)) posts))))

(define (topics-page r user)
  (define new-topic
    (if user
        `((p "Create new topic:") ,new-topic-form)
        '()))
  (page user
          "Topics"
          `((h1 "Topics")
            ,@new-topic
            ,(topics->table (all-topics r)))))

(define (topic-page r user tp)
  (match tp
    [(topic symbol name type)
     (define tagged-posts (get-posts r 'asc (with-tag symbol)))
     (define thread-posts (get-posts r 'asc (in-thread symbol)))
     (define tags (apply tags-hash r (map post-day (append thread-posts tagged-posts))))
     (page user name (topic-content user tp thread-posts tagged-posts tags))]))

(define (day-page r user dy)
  (define dstr (day->string dy))
  (match (and dy (get-post r dy))
    [#f (page user
              dstr
              `((h1 "There's no post for this day")
                ,@(if user
                      `((p (a ([href ,(day->url dy "/edit")]) "Edit.")))
                      '())))]
    [p (page user dstr `(,(post->section user p (all-topics r) (tags-hash r dy))))]))

(define (edit-post-page r user dy)
  (define dstr (day->string dy))
  (define p (get-post r dy))
  (define topics (all-topics r))
  (define tags (tags-hash r dy))
  (page user
        dstr
        `((h1 ,dstr)
          ,@(cond [p `((p "Existing post:")
                       ,(post->section user p topics tags)
                       ,@(tag-forms dy topics tags))]
                  [else `((p "There's no existing post for " (time ,dstr) "."))])
          ,@(day/post->forms dy p topics))))
