#| 

  Rumor in Racket/Gamble 

  From https://math.stackexchange.com/questions/12689/probability-on-spreading-of-rumors
  """
  A little help here. Exercise 21, Ch. 2 from Feller's book reads

  In a town [of] n+1 inhabitants, a person tells a rumor to a second person, who in 
  turn repeats it to a third person, etc. At each step, the recipient of the rumor 
  is chosen at random from the n people available. Find the probability that the rumor 
  will be told r times without:o 
  a) returning to the originator, 
  b) being repeated to any person. 
  Do the same problem when at each step the rumor is told by one person 
  to a gathering of N randomly chosen people. (The first question is the special case N=1).
  """

  Theoretical-0 for N=1:
  1: 1.0
  2: 0.8888888888888888
  3: 0.7901234567901234
  4: 0.7023319615912208
  5: 0.6242950769699741
  6: 0.5549289573066436
  7: 0.49327018427257213
  8: 0.4384623860200641
  9: 0.38974434312894585
  10: 0.34643941611461854

  (n 10 N 2 r 1 theoretical (for 0 case) 1.0)
  variable : p-0
  mean: 1.0

  variable : p-random
  mean: 1.0

  (n 10 N 2 r 3 theoretical (for 0 case) 0.6049382716049383)
  variable : p-0
  mean: 0.6004

  variable : p-random
  mean: 0.15910000000000002

  (n 10 N 3 r 1 theoretical (for 0 case) 1.0)
  variable : p-0
  mean: 1.0

  variable : p-random
  mean: 1.0

  (n 10 N 3 r 3 theoretical (for 0 case) 0.4444444444444444)
  variable : p-0
  mean: 0.44560000000000005

  variable : p-random
  mean: 0.0032000000000000006

  (n 10 N 5 r 1 theoretical (for 0 case) 1.0)
  variable : p-0
  mean: 1.0

  variable : p-random
  mean: 1.0

  (n 10 N 5 r 3 theoretical (for 0 case) 0.19753086419753085)
  variable : p-0
  mean: 0.1953

  variable : p-random
  mean: 0 (0.0)

 
  Cf gamble_rumor2.rkt for a simpler version (and a more complex representation).

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

; Here N=1
; Note that this is for n+1 people
; (define (theoretical-0 n r)
;  (expt (/ (- n 1) n) r))

; https://math.stackexchange.com/questions/1066150/probability-on-spreading-of-rumors-between-n1-people
(define (theoretical-0 n N r)
  (expt (- 1 (/ N n)) (- r 1)))

; https://math.stackexchange.com/questions/2302908/combinatorics-and-probability-of-rumour
; Is this correct? It's only for N=1 (there's no N in the formula)
; (define (theoretical-random n N r)
;   (/ (factorial n) (* (factorial (- n r)) (expt n r))))

; https://math.stackexchange.com/questions/4726319/probability-of-spread-of-rumors
; Only for N=1? For N>1 they don't match the model...
(define (theoretical-random n N r)
  (/ (* n N r) (expt (* n N) r)))
  
(displayln "Theoretical-0 for N=1:")
(for ([r (range 1 11)])
  (show r (* 1.0 (theoretical-0 (- 10 1) 1 r))))
(newline)
;
; If N > 1, then the rumor is told to N people at the same time.
; n: size of population
; N: number of people to spread the rumor at the each time
; r: Find the probability that the rumor will be told r times without
;    - a) returning to the originator (p-0)
;    - b) being repeated to any person (p-random)
;
(define (model n N r)
  (show2 "n" n "N" N "r" r
         ; The town has n+1 people so we adjust for that
         "theoretical (for 0 case)" (* 1.0 (theoretical-0 (- n 1) N r))
         ; "theoretical (for random)" (* 1.0 (theoretical-random (- n 1) N r))
         )
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define ps (range n))

   ; (define r 3)
   
   ; Check if anyone in els is a member in a
   (define (any-member els a)
     (ormap (lambda (v) (member v a)) els))

   ; Tell the rumor to N (other) people, not necessarily unique
   (define (draw p) (draw-without-replacement N (remove p ps)))

   (define (draw-until-member ps type)
     ; (newline)
     (define (loop i a)
       ; (show2 "loop i" i "a" a)
       (let* ([who (last a)]
              [ds (draw who)])
         ; (show "ds" ds)
         (if (or (and (eq? type "first") (member 0 ds))
                 (and (eq? type "any") (any-member ds a)))
             i
             (loop (add1 i) (append a ds))))
       )
     (loop 1 '(0))
     )

   ; Time when the speaker (0) hears the rumor again
   (define a-0 (draw-until-member ps "first"))
   
   ; Time any any person hears the rumor again
   (define a-random (draw-until-member ps "any"))

   (define p-0 (> a-0 r))
   (define p-random (> a-random r))   
   
   (list p-0
         p-random
         ; a-0
         ; a-random
         )
   
   )
)

(for* ([n (range 10 11)]
       [N '(2 3 5)]
       [r '(1 3)])
(show-marginals (model n N r)
                (list "p-0"
                      "p-random"
                      "a-0"
                      "a-random"
                      )
                #:num-samples 10000
                ; #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )
)
