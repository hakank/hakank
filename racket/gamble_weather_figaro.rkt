#| 

  Weather (Figaro) in Racket/Gamble 

  Weather model from "Practical Probabilistic Programming" 
  page 21ff
  Figaro code at page 24
  """

  Figaro output:
  """
  Today's greeting is "Hello, world!" with probability 0.27999999999999997.
  If today's greeting is "Hello, world!", today's weather is sunny with probability 0.4285714285714285.
  If today's greeting is "Hello, world!", tomorrow's greeting will be "Hello, world!" with probability 0.24857142857142858.
  """

  * observe_greeting: #f
  variable : sunnyToday
  #f: 4/5 (0.8)
  #t: 1/5 (0.2)
  mean: 1/5 (0.2)

  variable : greetingToday
  Oh no, not again: 16/25 (0.64)
  Hello, world!: 7/25 (0.28)
  Howdy, universe!: 2/25 (0.08)

  variable : sunnyTomorrow
  #f: 4/5 (0.8)
  #t: 1/5 (0.2)
  mean: 1/5 (0.2)

  variable : greetingTomorrow
  Oh no, not again: 18/25 (0.72)
  Hello, world!: 9/50 (0.18)
  Howdy, universe!: 1/10 (0.1)


  * observe_greeting: Hello, world!
  variable : sunnyToday
  #f: 4/7 (0.5714285714285714)
  #t: 3/7 (0.42857142857142855)
  mean: 3/7 (0.42857142857142855)

  variable : greetingToday
  Hello, world!: 1 (1.0)

  variable : sunnyTomorrow
  #f: 22/35 (0.6285714285714286)
  #t: 13/35 (0.37142857142857144)
  mean: 13/35 (0.37142857142857144)

  variable : greetingTomorrow
  Oh no, not again: 99/175 (0.5657142857142857)
  Hello, world!: 87/350 (0.24857142857142858)
  Howdy, universe!: 13/70 (0.18571428571428572)


  * observe_greeting: Howdy, universe!
  variable : sunnyToday
  #t: 1 (1.0)
  mean: 1 (1.0)

  variable : greetingToday
  Howdy, universe!: 1 (1.0)

  variable : sunnyTomorrow
  #t: 4/5 (0.8)
  #f: 1/5 (0.2)
  mean: 4/5 (0.8)

  variable : greetingTomorrow
  Hello, world!: 21/50 (0.42)
  Howdy, universe!: 2/5 (0.4)
  Oh no, not again: 9/50 (0.18)


  * observe_greeting: Oh no, not again
  variable : sunnyToday
  #f: 1 (1.0)
  mean: 0 (0.0)

  variable : greetingToday
  Oh no, not again: 1 (1.0)

  variable : sunnyTomorrow
  #f: 19/20 (0.95)
  #t: 1/20 (0.05)
  mean: 1/20 (0.05)

  variable : greetingTomorrow
  Oh no, not again: 171/200 (0.855)
  Hello, world!: 3/25 (0.12)
  Howdy, universe!: 1/40 (0.025)



  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model (observe_greeting #f))
  (newline)
  (show "* observe_greeting" observe_greeting)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define sunnyToday (flip 2/10))
   (define greetingToday
     (if sunnyToday 
         (categorical-vw2 (vector 6/10 4/10) (vector "Hello, world!"  "Howdy, universe!"))
         (categorical-vw2 (vector 2/10 8/10) (vector "Hello, world!" "Oh no, not again"))
         ))
        
   (define sunnyTomorrow (if sunnyToday (flip 8/10) (flip 5/100)))
   (define greetingTomorrow
     (if sunnyTomorrow 
         (categorical-vw2 (vector 5/10 5/10) (vector "Hello, world!" "Howdy, universe!"))
         (categorical-vw2 (vector 1/10 9/10) (vector "Hello, world!" "Oh no, not again"))))

   (when observe_greeting
     (observe/fail (eq? greetingToday observe_greeting))
     )

   (list sunnyToday
         greetingToday
         sunnyTomorrow
         greetingTomorrow
        )
   )
)


(for ([observe_greeting '(#f "Hello, world!"  "Howdy, universe!" "Oh no, not again")])
  (show-marginals (model observe_greeting)
                  (list  "sunnyToday"
                         "greetingToday"
                         "sunnyTomorrow"
                         "greetingTomorrow"
                     )
                  #:num-samples 1000
                  #:truncate-output 5
                  ; #:skip-marginals? #t
                  ; #:show-stats? #t
                  ; #:credible-interval 0.84
                  ; #:hpd-interval (list 0.84)
                  ; #:show-histogram? #t
                  ; #:show-percentiles? #t
                  ; #:burn 0
                  ; #:thin 0
                  )
  )


