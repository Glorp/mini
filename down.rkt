#lang racket/base
(require racket/match
         racket/string)

(provide parsedown)

(define (line? str)
  (non-empty-string? (string-trim str)))

(define (blank? str)
  (not (line? str)))

(define (start lst res)
  (match lst
    ['() res]
    [(list (? blank?) rest ...) (start rest res)]
    [(list line rest ...) (p rest res (list line))]))

(define (p lst res par)
  (match lst
    ['() (cons (cons 'p (reverse par)) res)]
    [(list (? blank?) rest ...) (start rest (cons (cons 'p (reverse par)) res))]
    [(list line rest ...) (p rest res (list* line '(br)  par))]))

(define (parsedown str)
  (reverse (start (string-split str "\n") '())))

    
(module+ test
  (require rackunit)
  (check-equal? (parsedown "") '())
  (check-equal? (parsedown "\n \t \n ") '())
  (check-equal? (parsedown "foo") '((p "foo")))
  (check-equal? (parsedown "\n\nfoo") '((p "foo")))
  (check-equal? (parsedown "foo\n\n\n") '((p "foo")))
  (check-equal? (parsedown "\n\nfoo\n\n\n") '((p "foo")))
  (check-equal? (parsedown "foo\nbar") '((p "foo" (br) "bar")))
  (check-equal? (parsedown "foo\n\nbar") '((p "foo") (p "bar")))
  (check-equal? (parsedown "\n\nfoo\nfaa\n\nbar\nbear\nboor\n\n\n\nmlep\n")
                '((p "foo" (br) "faa") (p "bar" (br) "bear" (br) "boor") (p "mlep"))))

