#lang racket/base
(require racket/match
         racket/list
         racket/string
         racket/format
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
         get-topic
         all-topics
         tag
         untag
         tags-hash)

(struct repo (connection topics) #:mutable)

(define (open-repo path)
  (define c (sqlite3-connect #:database path #:mode 'create))
  (query-exec c (~a "CREATE TABLE IF NOT EXISTS topic("
                    "  symbol TEXT PRIMARY KEY,"
                    "  name TEXT,"
                    "  type INTEGER" 
                    ")"
                    #:separator "\n"))
  (query-exec c (~a "CREATE TABLE IF NOT EXISTS post("
                    "  day TEXT PRIMARY KEY,"
                    "  text TEXT,"
                    "  symbol INTEGER,"
                    "  link TEXT,"
                    "  FOREIGN KEY(symbol) REFERENCES topic(symbol)"
                    ")"
                    #:separator "\n"))
  (query-exec c "CREATE INDEX IF NOT EXISTS post_symbol ON post(symbol)")
  (query-exec c (~a "CREATE TABLE IF NOT EXISTS tagged("
                    "  day TEXT,"
                    "  symbol TEXT,"
                    "  FOREIGN KEY(day) REFERENCES post(day),"
                    "  FOREIGN KEY(symbol) REFERENCES topic(symbol),"
                    "  UNIQUE(day, symbol) ON CONFLICT IGNORE"
                    ")"
                    #:separator "\n"))
  (query-exec c (~a "CREATE INDEX IF NOT EXISTS tagged_day ON tagged(day)"))
  (query-exec c (~a "CREATE INDEX IF NOT EXISTS tagged_symbol ON tagged(symbol)"))  
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
    [(post day text symbol link)
     (query-exec (con rp)
                 "INSERT INTO post (day, text, symbol, link) VALUES ($1, $2, $3, $4)"
                 (day->string day)
                 text
                 (false->sql-null (and symbol (symbol->string symbol)))
                 (false->sql-null link))]))

(define (update-post rp pst)
  (match pst
    [(post day text symbol link)
     (query-exec (con rp)
                 "UPDATE post SET text = $1, symbol = $2, link = $3 WHERE day = $4"
                 text
                 (false->sql-null (and symbol (symbol->string symbol)))
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
                           "SELECT day, text, symbol, link FROM post WHERE day = $1"
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
        [(in-thread symbol) (list (format "symbol = $~a" n) (symbol->string symbol))]
        [(with-tag symbol) (list (format "day IN (SELECT day FROM tagged WHERE symbol = $~a)" n)
                                 (symbol->string symbol))])))
  (define where
    (if (empty? clauses)
        ""
        (string-join (map first clauses) " AND " #:before-first " WHERE ")))
  (define values (map second clauses))
  (define rows
    (apply query-rows
           (con rp)
           (format "SELECT day, text, symbol, link FROM post~a ORDER BY day ~a" where order)
           values))
  (map row->post rows))

(define (type->int type)
  (match type
    ['neither 0]
    ['thread 1]
    ['tag 2]
    ['either 3]))
(define (int->type type)
  (match type
    [0 'neither]
    [1 'thread]
    [2 'tag]
    [3 'either]))

(define (create-topic rp tp)
  (set-repo-topics! rp #f)
  (match tp
    [(topic symbol name type)
     (query-exec (con rp)
                 "INSERT INTO topic (symbol, name, type) VALUES ($1, $2, $3)"
                 (symbol->string symbol)
                 name
                 (type->int type))]))

(define (update-topic rp tp)
  (set-repo-topics! rp #f)
  (match tp
    [(topic symbol name type)
     (query-exec (con rp)
                 "UPDATE topic SET name = $1, type = $2 WHERE symbol = $3"
                 name
                 (type->int type)
                 (symbol->string symbol))]))

(define (row->topic row)
  (match row
    [(vector id name type) (topic (string->symbol id) name (int->type type))]))

(define (all-topics rp)
  (unless (repo-topics rp)
    (define all (map row->topic
                     (query-rows (con rp)
                                 "SELECT symbol, name, type FROM topic ORDER BY name, symbol")))
    (define ((has-type? type) tp)
      (match tp
        [(topic _ _ 'either) #t]
        [(topic _ _ symbol) (eq? symbol type)]))
    (set-repo-topics! rp
                      (topics (make-immutable-hasheq (map (λ (t) (cons (topic-symbol t) t)) all))
                              all
                              (filter (has-type? 'thread) all)
                              (filter (has-type? 'tag) all))))
  (repo-topics rp))

(define (get-topic rp id)
  (hash-ref (topics-hash (all-topics rp)) id))

(define (tag rp p tp)
  (match* (p tp)
    [((post dy _ _ _) (topic sym _ _))
     (query-exec (con rp)
                 "INSERT INTO tagged (day, symbol) VALUES ($1, $2)"
                 (day->string dy)
                 (symbol->string sym))]))

(define (untag rp p tp)
  (match* (p tp)
    [((post dy _ _ _) (topic sym _ _))
     (query-exec (con rp)
                 "DELETE FROM tagged WHERE day = $1 AND symbol = $2"
                 (day->string dy)
                 (symbol->string sym))]))

(define (tags-hash rp . days)
  (define in (string-join (map (λ (dy) (format "'~a'" (day->string dy))) days) ", "))
  (define tphash (topics-hash (all-topics rp)))
  (define rows (query-rows (con rp)
                           (~a "SELECT day, tagged.symbol"
                               "FROM tagged JOIN topic ON tagged.symbol = topic.symbol"
                               (format "WHERE day IN (~a)" in)
                               "ORDER BY topic.name DESC"
                               #:separator "\n")))
  (for/fold ([h (hash)]) ([row rows])
    (match row
      [(vector dystr tag)
       (define dy (string->day dystr))
       (define tp (hash-ref tphash (string->symbol tag)))
       (hash-set h dy (cons tp (hash-ref h dy '())))])))

(module+ test
  (require (except-in rackunit before after))
  (define r (open-repo 'memory))
  
  (create-topic r (topic 'hello "Hello" 'either))
  (define t (topic 'hello "Hi" 'thread))
  (check-exn exn:fail:sql? (λ () (create-topic r t)))
  (check-equal? (get-topic r 'hello) (topic 'hello "Hello" 'either))
  (update-topic r t)
  (check-equal? (get-topic r 'hello) t)
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
                (list p3))
  (tag r p1 t)
  (tag r p2 t)
  (tag r p3 t)
  (define tl (list t))
  (check-equal? (tags-hash r d1 d3) (hash d1 tl d3 tl))
  (check-equal? (tags-hash r d1 d2 d3) (hash d1 tl d2 tl d3 tl)))
