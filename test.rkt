#lang racket/base
(require web-server/servlet-env
         "servlet.rkt"
         "repo.rkt"
         "day.rkt"
         "post.rkt")

(define the-day (today))

(define r (open-repo 'memory))
(define t (topic 'important-topic "An important topic" 'thread))
(create-topic r t)
(create-post r (post (add-days the-day -10)
                     "lorem ipswitch\nbla _bli_ `blablah`\n\nboop."
                     'important-topic
                     "https://dailyotter.org"))
(define p1 (post (add-days the-day -5) "beep\nboop\nbap" #f "https://dailybunny.org"))
(define p2 (post (add-days the-day -3) "blep\nblop\nblap" 'important-topic #f))
(define p3 (post (add-days the-day -2) "mlep\nmlop\nmlap" #f #f))
(define p4 (post (add-days the-day -1) "BEPP" 'beep #f))
(create-post r p1)
(create-post r p2)
(create-post r p3)
(create-post r p4)
(define tag1 (topic 'beep "Beep" 'tag))
(define tag2 (topic 'boop "Boop" 'tag))
(create-topic r tag1)
(create-topic r tag2)
(tag r p1 tag1)
(tag r p1 tag2)
(tag r p2 tag1)

(serve/servlet (servlet r (λ (user pwd) #t) today)
               #:stateless? #t
               #:servlet-regexp #rx""
               #:servlet-path "/login")