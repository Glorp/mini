#lang racket/base
(require (only-in racket/match match)
         (only-in racket/string non-empty-string? string-trim string-split))

(provide parsedown
         parseline)

(define (parseline str)
  (reverse (parseline-priv str '())))

(define (parseline-priv str res)
  (define (mode-str? s)
    (or (equal? "_" s) (equal? "`" s)))
  (define (mode-symbol s)
    (match s
      ["_" 'em]
      ["`" 'code]))
  (define (mode m l res)
    (match l
      ['() (list res '())]
      [(list (? (λ (s) (equal? m s))) rest ...) (list res rest)]
      [(list (regexp #px"\\\\(.?)" (list _ s)) rest ...) (mode m rest (cons s res))]
      [(list s rest ...) (mode m rest (cons s res))]))
  (define (halp l res)
    (match l
      ['() res]
      [(list (? mode-str? m) rest ...)
       (match (mode m rest '())
         [(list m-res rest) (halp rest (cons (cons (mode-symbol m) (reverse m-res)) res))])]
      [(list (regexp #px"\\\\(.?)" (list _ s)) rest ...) (halp rest (cons s res))]
      [(list s rest ...) (halp rest (cons s res))]))
  (halp (regexp-match* #px"[\\\\].|[\\\\]$|[_`]|[^\\\\_`]+" str) res))

(define (line? str)
  (non-empty-string? (string-trim str)))

(define (blank? str)
  (not (line? str)))

(define (start lst res)
  (match lst
    ['() res]
    [(list (? blank?) rest ...) (start rest res)]
    [(list line rest ...) (p rest res (parseline-priv line '()))]))

(define (p lst res par)
  (match lst
    ['() (cons (cons 'p (reverse par)) res)]
    [(list (? blank?) rest ...) (start rest (cons (cons 'p (reverse par)) res))]
    [(list line rest ...) (p rest res (parseline-priv line (cons '(br)  par)))]))

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
                '((p "foo" (br) "faa") (p "bar" (br) "bear" (br) "boor") (p "mlep")))
  (check-equal? (parsedown "foo _bar_ `blep\\blop`asd`ewe\n\nasd _erm")
                '((p "foo " (em "bar") " " (code "blep" "b" "lop") "asd" (code "ewe"))
                  (p "asd " (em "erm")))))

