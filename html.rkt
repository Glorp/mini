#lang racket/base
(require racket/match
         racket/format
         racket/set
         racket/list)

(provide write-html html doctype)

(define normal-elements
  (seteq 'html 'head 'title 'body 'h1 'h2 'h3 'p 'a 'em 'strong 'div 'pre 'code 'figure 'figcaption
         'time 'form 'label 'textarea 'section 'header 'footer 'nav 'table 'thead 'tr 'th 'td
         'select 'datalist 'option))
(define void-elements
  (seteq 'br 'hr 'img 'input 'link 'meta))
(define normal-attributes
  (seteq 'lang 'charset 'rel 'alt 'href 'id 'src 'type 'action 'name 'value 'method 'colspan 'list
         'label 'selected))
(define boolean-attributes
  (seteq 'checked))

(define doctype "<!DOCTYPE html>")

(define escape-table #px"[<>&]")
(define escape-attribute-table #rx"[<>&\"]")

(define (replace-escaped s)
  (define c (string-ref s 0))
  (case c
    [(#\<) "&lt;"]
    [(#\>) "&gt;"]
    [(#\&) "&amp;"]
    [(#\") "&quot;"]
    [else c]))

(define (escape x table)
  (regexp-replace* table x replace-escaped))

(define (write-escaped str table out)
  (cond [(regexp-match table str)
         (write-string (escape str table) out)]
        [else
         (write-string str out)]))

(define (write-symbol sym out)
  (write-string (symbol->string sym) out))

(define (normal-element? x)
  (set-member? normal-elements x))
(define (void-element? x)
  (set-member? void-elements x))
(define (normal-attribute? x)
  (set-member? normal-attributes x))
(define (boolean-attribute? x)
  (set-member? boolean-attributes x))

(define (write-attr attr out)
  (write-string " " out)
  (match attr
    [(list (? normal-attribute? name) (? string? value))
     (write-symbol name out)
     (write-string "=\"" out)
     (write-escaped value escape-attribute-table out)
     (write-string "\"" out)]
    [(? boolean-attribute? name) (write-symbol name out)]
    [_ (error 'write-attr "bad attribute: ~a" attr)]))

(define (start-tag name attrs out)
  (write-char #\< out)
  (write-symbol name out)
  (for ([attr attrs])
    (write-attr attr out)))

(define (write-normal-element tag-name attrs children out)
  (start-tag tag-name attrs out)
  (write-char #\> out)
  (for ([child children])
    (write-html child out))
  (write-string "</" out)
  (write-symbol tag-name out)
  (write-char #\> out))

(define (write-void-element tag-name attrs out)
  (start-tag tag-name attrs out)
  (write-char #\> out))

(define (attributes? x)
  (or (null? x) (and (list? x) (list? (first x)))))

(define (write-element tag-name attrs children out)
  (cond [(normal-element? tag-name) (write-normal-element tag-name attrs children out)]
        [else (error 'write-html "bad (normal/non-void) tag name: ~a" tag-name)]))

(define (write-empty-element tag-name attrs out)
  (cond [(void-element? tag-name) (write-void-element tag-name attrs out)]
        [else (write-element tag-name attrs '() out)]))

(define (write-html content out)
  (match content
    [(? string? str) (write-escaped str escape-table out)]
    [(list tag-name (? attributes? attrs))
     (write-empty-element tag-name attrs out)]
    [(list tag-name (? attributes? attrs) children ...)
     (write-element tag-name attrs children out)]
    [(list tag-name)
     (write-empty-element tag-name '() out)]
    [(list tag-name children ...)
     (write-element tag-name '() children out)]
    [_ (error 'write-html "bad content: ~a" content)]))

(define (html x)
  (define out (open-output-string))
  (write-html x out)
  (get-output-string out))

(module+ test
  (require rackunit)
  (check-equal? (html '(hr ())) "<hr>")
  (check-equal? (html '(hr)) "<hr>")
  (check-equal? (html '(div ())) "<div></div>")
  (check-equal? (html '(div)) "<div></div>")
  (check-equal? (html '(p "ble<p")) "<p>ble&lt;p</p>")
  (check-equal? (html '(a ([href "./foo.html"]) "blap")) "<a href=\"./foo.html\">blap</a>")
  (check-equal? (html "ab<cd>ef&gh\"ij") "ab&lt;cd&gt;ef&amp;gh\"ij")
  (check-equal? (html '(p ([id "ab<cd>ef&gh\"ij"]))) "<p id=\"ab&lt;cd&gt;ef&amp;gh&quot;ij\"></p>")
  (check-equal?
   (html '(div ()
               (h1 ([id "stuf"]) "Some " (em () "stuff"))
               (p "Stuff incoming.")
               (hr)
               (p "Here's the stuff:")
               (img ([src "./blap.png"] [alt "A thing"]))))
   (~a "<div><h1 id=\"stuf\">Some <em>stuff</em></h1><p>Stuff incoming.</p><hr>"
                  "<p>Here's the stuff:</p><img src=\"./blap.png\" alt=\"A thing\"></div>"))
  (check-equal?
   (html
    '(html
      ([lang "en"])
      (head (meta ([charset "utf-8"])) (title "welp"))
      (body (h1 "hello") (p "hi"))))
   (~a "<html lang=\"en\"><head><meta charset=\"utf-8\"><title>welp</title></head>"
                  "<body><h1>hello</h1><p>hi</p></body></html>")))
