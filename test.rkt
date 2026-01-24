#lang racket/base
(require (only-in web-server/servlet-env serve/servlet)
         (only-in racket/format ~a)
         "servlet.rkt"
         "repo.rkt"
         "day.rkt"
         "post.rkt")

(define the-day (today))
(define r (open-repo 'memory))
(define animals (topic 'animals "Animals" 'either) )
(create-topic r animals)
(create-post r (post (add-days the-day -7) "Animals are cute" 'animals #f))

(define lagomorphs (topic 'lagomorphs "Lagomorphs " 'tag))
(create-topic r lagomorphs)
(define bunnies
  (post
   (add-days the-day -6)
   (~a
    "Rabbits, or bunnies, are small mammals in the family Leporidae (which also includes the hares), "
    "which is in the order Lagomorpha (which also includes pikas). They are familiar throughout the "
    "world as a small herbivore, a prey animal, a domesticated form of livestock, and a pet, having "
    "a widespread effect on ecologies and cultures.\n\n"
    "Rabbits have ears.")
   #f
   "https://dailybunny.org"))
(create-post r bunnies)
(tag r bunnies lagomorphs)
(tag r bunnies animals)

(define england (topic 'england "England" 'tag))
(create-topic r england)
(define ipswitch (post (add-days the-day -5) "lorem ipswitch bla bla bla" #f #f))
(create-post r ipswitch)
(tag r ipswitch england)

(create-topic r (topic 'otters "Otters" 'thread))

(define otters
  (post
   (add-days the-day -4)
   (~a
    "Otters are carnivorous mammals in the subfamily Lutrinae. The 14 extant otter species are all "
    "semiaquatic, both freshwater and marine. Lutrinae is a branch of the Mustelidae family, which "
    "includes weasels, badgers, mink, and wolverines, among other animals.")
   'otters
   "https://dailyotter.org"))
(create-post r otters)
(tag r otters animals)

(create-post
 r
 (post
  (add-days the-day -3)
  (~a
   "Otters are distinguished by their long, slim bodies, powerful webbed feet for swimming, and "
   "their dense fur, which keeps them warm and buoyant in water. They are playful animals, engaging "
   "in activities like sliding into water on natural slides and playing with stones.")
  'otters
  #f))

(create-post r (post (add-days the-day -2) "bla bla blah" #f #f))
(create-post
 r
 (post
  (add-days the-day -1)
  (~a
   "Otters exhibit a varied life cycle with a gestation period of about 60–86 days, and offspring "
   "typically stay with their family for a year. They can live up to 16 years, with their diet "
   "mainly consisting of fish and sometimes frogs, birds, or shellfish, depending on the species.")
  'otters
  #f))

(serve/servlet (servlet r (λ (user pwd) #t) today)
               #:stateless? #t
               #:servlet-regexp #rx""
               #:servlet-path "/login")