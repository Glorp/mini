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
         get-posts
         create-topic
         update-topic
         get-topic)

(struct repo (connection topics)
  #:mutable)

(define (open-repo path)
  (define c (sqlite3-connect
             #:database path
             #:mode 'create))

  (query-exec c (string-append "CREATE TABLE IF NOT EXISTS topic(\n"
                               "  id TEXT PRIMARY KEY,\n"
                               "  name TEXT\n"
                               ")"))
  (query-exec c (string-append "CREATE TABLE IF NOT EXISTS post(\n"
                               "  day TEXT PRIMARY KEY,\n"
                               "  text TEXT,\n"
                               "  id INTEGER,\n"
                               "  link TEXT,\n"
                               "  FOREIGN KEY(id) REFERENCES topic(id)"
                               ")"))
  (query-exec c (string-append "CREATE TABLE IF NOT EXISTS tagged(\n"
                               "  day TEXT,\n"
                               "  id TEXT\n,"
                               "  FOREIGN KEY(day) REFERENCES post(day),\n"
                               "  FOREIGN KEY(id) REFERENCES topic(id)"
                               ")"))
  (repo c #f))

(define (close-repo rp)
  (define con (repo-connection rp))
  (when con
    (disconnect rp)
    (set-repo-connection! #f)))  

(define (con rp)
  (define c (repo-connection rp))
  (unless c
    (error 'con "database connection is closed"))
  c)

(define (create-post rp pst)
  (match pst
    [(post day text id link)
     (query-exec (con rp)
                 "INSERT INTO post (day, text, id, link) VALUES ($1, $2, $3, $4)"
                 (day->string day)
                 text
                 (false->sql-null (and id (symbol->string id)))
                 (false->sql-null link))]))

(define (update-post rp pst)
  (match pst
    [(post day text id link)
     (query-exec (con rp)
                 "UPDATE post SET text = $1, id = $2, link = $3 WHERE day = $4"
                 text
                 (false->sql-null (and id (symbol->string id)))
                 (false->sql-null link)
                 (day->string day))]))

(define (row->post row)
  (match row
    [(vector day text id link)
     (define maybe-id (sql-null->false id))
     (post (string->day day)
           text
           (and maybe-id (string->symbol maybe-id))
           (sql-null->false link))]))

(define (get-post rp day)
  (define rows (query-rows (con rp)
                           "SELECT day, text, id, link FROM post WHERE day = $1"
                           (day->string day)))
  (match rows
    ['() #f]
    [(list row) (row->post row)]
    [_ (error 'get-post "weird result: ~a" rows)]))

(define (get-posts rp direction . filters)
  (define order
    (match direction
      ['asc "ASC"]
      ['desc "DESC"]))
  (define clauses
    (for/list ([filter filters] [n (in-naturals 1)])
      (match filter
        [(before d) (list (format "day < $~a" n) (day->string d))]
        [(after d) (list (format "day > $~a" n) (day->string d))]
        [(in-thread d) (list (format "id = $~a" n) (symbol->string d))])))
  (define where
    (if (empty? clauses)
        ""
        (string-join (map first clauses) " AND " #:before-first " WHERE ")))
  (define values (map second clauses))
  (define rows
    (apply query-rows
           (con rp)
           (format "SELECT day, text, id, link FROM post~a ORDER BY day ~a" where order)
           values))
  (map row->post rows))

(define (create-topic rp tp)
  (set-repo-topics! rp #f)
  (match tp
    [(topic id name)
     (query-exec (con rp)
                 "INSERT INTO topic (id, name) VALUES ($1, $2)"
                 (symbol->string id)
                 name)]))

(define (update-topic rp tp)
  (set-repo-topics! rp #f)
  (match tp
    [(topic id name)
     (query-exec (con rp) "UPDATE topic SET name = $1 WHERE id = $2" name (symbol->string id))]))

(define (row->topic row)
  (match row
    [(vector id name) (topic (string->symbol id) name)]))

(define (all-topics rp)
  (unless (repo-topics rp)
    (define list (map row->topic
                        (query-rows (con rp) "SELECT id, name FROM topic ORDER BY name, id")))
    (set-repo-topics! rp
                      (topics list
                              (make-immutable-hasheq (map (λ (t) (cons (topic-id t) t)) list)))))
  (repo-topics rp))

(define (get-topic rp id)
  (hash-ref (topics-hash (all-topics rp)) id))

(module+ test
  (require (except-in rackunit before after))
  (define r (open-repo 'memory))
  
  (create-topic r (topic 'hello "Hello"))
  (check-equal? (get-topic r 'hello) (topic 'hello "Hello"))
  (check-exn exn:fail:sql? (λ () (create-topic r (topic 'hello "Hi"))))
  (update-topic r (topic 'hello "Hi"))
  (check-equal? (get-topic r 'hello) (topic 'hello "Hi"))
  (define d1 (day 2026 01 01))
  (define d2 (day 2026 02 01))
  (define p1 (post d1 "beep" 'hello #f))
  (define p2 (post d2 "boop" #f #f))
  (create-post r p1)
  (create-post r p2)
  (check-equal? (get-post r d1) p1)
  (check-equal? (get-post r d2) p2)
  (define d3 (day 2026 01 02))
  (create-post r (post d3 "bep" #f #f))
  (check-equal? (get-posts r 'asc (in-thread 'hello)) (list p1))
  (define p3 (post d3 "bep" 'hello #f))
  (check-exn exn:fail:sql? (λ () (create-post r p3)))
  (check-equal? (get-post r d3) (post d3 "bep" #f #f))
  (update-post r p3)
  (check-equal? (get-post r d3) p3)
  (define all (list p1 p3 p2))
  (check-equal? (get-posts r 'asc (in-thread 'hello)) (list p1 p3))
  (check-equal? (get-posts r 'asc) all)
  (check-equal? (get-posts r 'desc) (reverse all))
  (check-equal? (get-posts r 'asc (after (day 2026 01 01))) (rest all))
  (check-equal? (get-posts r 'desc (before (day 2026 01 03))) (rest (reverse all)))
  (check-equal? (get-posts r
                           'desc
                           (before (day 2026 01 03))
                           (after (day 2026 01 01)))
                (list p3)))
