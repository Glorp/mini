#lang racket/base
(require racket/match
         racket/list
         racket/string
         db
         "day.rkt"
         "post.rkt")

(provide open-repo
         close-repo
         create-post
         update-post
         get-post
         get-posts)

(struct repo ([connection #:mutable]))

(define (open-repo path)
  (define c (sqlite3-connect
             #:database path
             #:mode 'create))
  (query-exec c (string-append "CREATE TABLE IF NOT EXISTS post(\n"
                               "  day TEXT PRIMARY KEY,\n"
                               "  text TEXT,\n"
                               "  thread TEXT,\n"
                               "  FOREIGN KEY(thread) REFERENCES post(day)"
                               ")"))
  (query-exec c (string-append "CREATE TABLE IF NOT EXISTS thread(\n"
                               "  day TEXT PRIMARY KEY,\n"
                               "  title TEXT,\n"
                               "  FOREIGN KEY(day) REFERENCES post(day)"
                               ")"))
  (repo c))

(define (close-repo repo)
  (define con (repo-connection repo))
  (when con
    (disconnect repo)
    (set-repo-connection! #f)))  

(define (con repo)
  (define c (repo-connection repo))
  (unless c
    (error 'con "database connection is closed"))
  c)

(define (create-post repo pst)
  (match pst
    [(post day text thread)
     (query-exec (con repo)
                 "INSERT INTO post (day, text, thread) VALUES ($1, $2, $3)"
                 (day->string day)
                 text
                 (false->sql-null (maybe-day->string thread)))]))

(define (update-post repo pst)
  (match pst
    [(post day text thread)
     (query-exec (con repo) "UPDATE post SET text = $1, thread = $2 WHERE day = $3"
                 text
                 (false->sql-null (maybe-day->string thread))
                 (day->string day))]))

(define (row->post row)
  (match row
    [(vector day text thread)
     (post (string->day day)
           text
           (if (sql-null? thread)
               #f
               (string->day thread)))]))

(define (get-post repo day)
  (define rows (query-rows (con repo)
                           "SELECT day, text, thread FROM post WHERE day = $1"
                           (day->string day)))
  (match rows
    ['() #f]
    [(list row) (row->post row)]
    [_ (error 'get-post "weird result: ~a" rows)]))

(define (get-posts repo direction . filters)
  (define order
    (match direction
      ['asc "ASC"]
      ['desc "DESC"]))
  (define clauses
    (for/list ([filter filters] [n (in-naturals 1)])
      (match filter
        [(before d) (list (format "day < $~a" n) (day->string d))]
        [(after d) (list (format "day > $~a" n) (day->string d))]
        [(in-thread d) (list (format "thread = $~a" n) (day->string d))])))
  (define where
    (if (empty? clauses)
        ""
        (string-join (map first clauses) " AND " #:before-first " WHERE ")))
  (define values (map second clauses))
  (define rows
    (apply query-rows
           (con repo)
           (format "SELECT day, text, thread FROM post~a ORDER BY day ~a" where order)
           values))
  (map row->post rows))

(module+ test
  (require (except-in rackunit before after))
  (define r (open-repo 'memory))
  (define d1 (day 2026 01 01))
  (define d2 (day 2026 02 01))
  (define p1 (post d1 "beep" d1))
  (define p2 (post d2 "boop" #f))
  (create-post r p1)
  (create-post r p2)
  (check-equal? (get-post r d1) p1)
  (check-equal? (get-post r d2) p2)

  (define d3 (day 2026 01 02))
  (create-post r (post d3 "bep" #f))
  (check-equal? (get-posts r 'asc (in-thread d1)) (list p1))
  
  (define p3 (post d3 "bep" d1))
  (check-exn exn:fail:sql? (λ () (create-post r p3)))
  (check-equal? (get-post r d3) (post d3 "bep" #f))
  (update-post r p3)
  (check-equal? (get-post r d3) p3)
  (define all (list p1 p3 p2))
  (check-equal? (get-posts r 'asc (in-thread d1)) (list p1 p3))
  (check-equal? (get-posts r 'asc) all)
  (check-equal? (get-posts r 'desc) (reverse all))
  (check-equal? (get-posts r 'asc (after (day 2026 01 01))) (rest all))
  (check-equal? (get-posts r 'desc (before (day 2026 01 03))) (rest (reverse all)))
  (check-equal? (get-posts r
                           'desc
                           (before (day 2026 01 03))
                           (after (day 2026 01 01)))
                (list p3)))
