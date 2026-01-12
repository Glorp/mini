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

  (query-exec c (string-append "CREATE TABLE IF NOT EXISTS topic(\n"
                               "  id INTEGER PRIMARY KEY,\n"
                               "  name TEXT\n"
                               ")"))
  (query-exec c (string-append "CREATE TABLE IF NOT EXISTS post(\n"
                               "  day TEXT PRIMARY KEY,\n"
                               "  text TEXT,\n"
                               "  topic_id INTEGER,\n"
                               "  FOREIGN KEY(topic_id) REFERENCES topic(id)"
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
    [(post day text topic-id)
     (query-exec (con repo)
                 "INSERT INTO post (day, text, topic_id) VALUES ($1, $2, $3)"
                 (day->string day)
                 text
                 (false->sql-null topic-id))]))

(define (update-post repo pst)
  (match pst
    [(post day text topic-id)
     (query-exec (con repo) "UPDATE post SET text = $1, topic_id = $2 WHERE day = $3"
                 text
                 (false->sql-null topic-id)
                 (day->string day))]))

(define (row->post row)
  (match row
    [(vector day text topic-id)
     (post (string->day day)
           text
           (sql-null->false topic-id))]))

(define (get-post repo day)
  (define rows (query-rows (con repo)
                           "SELECT day, text, topic_id FROM post WHERE day = $1"
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
        [(in-topic d) (list (format "topic_id = $~a" n) d)])))
  (define where
    (if (empty? clauses)
        ""
        (string-join (map first clauses) " AND " #:before-first " WHERE ")))
  (define values (map second clauses))
  (define rows
    (apply query-rows
           (con repo)
           (format "SELECT day, text, topic_id FROM post~a ORDER BY day ~a" where order)
           values))
  (map row->post rows))

(define (create-topic repo name)
  (unless
      (string? name)
    (error 'create-topic "name must be string: ~s" name))
  (topic (query-value (con repo) "INSERT INTO topic (name) VALUES ($1) RETURNING id" name) name))

(define (update-topic repo tp)
  (match tp
    [(topic id name)
     (query-exec (con repo) "UPDATE topic SET name = $1 WHERE id = $1" name id)]))

(define (row->topic row)
  (topic (vector-ref row 0) (vector-ref row 1)))

(define (get-topic topic-id)
  (row->topic (query-row (con repo) "SELECT id, name FROM topic WHERE id = $1" topic-id)))

(module+ test
  (require (except-in rackunit before after))
  (define r (open-repo 'memory))
  (define t (create-topic r "Hello"))
  (check-equal? (topic-name t) "Hello")
  (define d1 (day 2026 01 01))
  (define d2 (day 2026 02 01))
  (define p1 (post d1 "beep" 0))
  (define p2 (post d2 "boop" #f))
  (create-post r p1)
  (create-post r p2)
  (check-equal? (get-post r d1) p1)
  (check-equal? (get-post r d2) p2)

  (define d3 (day 2026 01 02))
  (create-post r (post d3 "bep" #f))
  (check-equal? (get-posts r 'asc (in-topic 0)) (list p1))
  
  (define p3 (post d3 "bep" 0))
  (check-exn exn:fail:sql? (λ () (create-post r p3)))
  (check-equal? (get-post r d3) (post d3 "bep" #f))
  (update-post r p3)
  (check-equal? (get-post r d3) p3)
  (define all (list p1 p3 p2))
  (check-equal? (get-posts r 'asc (in-topic 0)) (list p1 p3))
  (check-equal? (get-posts r 'asc) all)
  (check-equal? (get-posts r 'desc) (reverse all))
  (check-equal? (get-posts r 'asc (after (day 2026 01 01))) (rest all))
  (check-equal? (get-posts r 'desc (before (day 2026 01 03))) (rest (reverse all)))
  (check-equal? (get-posts r
                           'desc
                           (before (day 2026 01 03))
                           (after (day 2026 01 01)))
                (list p3)))
