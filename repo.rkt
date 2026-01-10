#lang racket/base
(require db)

(provide open-repo
         close-repo)

(struct repo ([connection #:mutable]))

(define (open-repo path)
  (define c (sqlite3-connect
             #:database path
             #:mode 'create))
  #;(query-exec c
              "CREATE TABLE IF NOT EXISTS post (id INTEGER PRIMARY KEY AUTOINCREMENT, content TEXT)")
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

;(query-exec con "INSERT INTO post (content) VALUES ($2)" (rndstring 20 120))

#;(query con "SELECT id, content FROM post ORDER BY id")
