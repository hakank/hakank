#| 

  Barber problem Racket Gamble.
  
  From cplint:
  """
  Example 16. The barber paradox, introduced by Bertrand Russell [42], is expressed as:
  The village barber shaves everyone in the village who does not shave himself.
  The paradox was modeled as a logic program under WFS in [43]. Making things
  probabilistic, the paradox can be modeled as the LPAD:
  """

  The result from my cplint model barber.pl
  """
  ?- findall([X,shaves,Y,with,P], prob(shaves(X,Y),P),L).
  L = [[barber, shaves, mayor, with, 1.0], 
       [barber, shaves, barber, with, 0.25], 
       [barber, shaves, doctor, with, 0.75], 
       [doctor, shaves, doctor, with, 0.25]].
  """
  (Note that "mayor, shaves, mayor" is missing here, so we assume the probability is 0).


  The result from my PSI model barber.psi:

    shaves_barber_barber  1/4
    shaves_doctor_doctor  1/4
    shaves_barber_doctor  1 
    shaves_barber_mayor   1 
    shaves_mayor_mayor    0


  And this model give (about) the same result.

var : barber shaves barber
#f: 0.7499
#t: 0.2501
mean: 0.2501

var : doctor shaves doctor
#f: 0.7495
#t: 0.2505
mean: 0.2505

var : barber shaves doctor
#t: 1.0
mean: 1.0

var : barber shaves mayor
#t: 1.0
mean: 1.0

var : mayor shaves mayor
0: 1.0
mean: 0 (0.0)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

;; Special "2d" setter/getter
;; The 
(define (my-vector-set! v n i j val)
  (vector-set! v (+ (* i n) j) val))

(define (my-vector-ref2d v n i j)
  (vector-ref v (+ (* i n) j)))

; set! model
(define (model)

  (; enumerate ; strange results
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
   (define barber 0)
   (define doctor 1)
   (define mayor 2)
   (define n 3)

   (define shaves (make-vector 9 0))
   
   ; barber and doctor shaves themselves with probability 0.25
   ; all other with p 0.5
   (for ([p1 n])
     (for ([p2 n])
       (if (or (and (= p1 barber) (= p2 barber))
               (and (= p1 doctor) (= p2 doctor)))
           (my-vector-set! shaves n p1 p2 (flip 0.25))
           (when (= p1 barber)
               (my-vector-set! shaves n p1 p2 #t))
           )
       )
     )
  
   (list (my-vector-ref2d shaves 3 barber barber)
         (my-vector-ref2d shaves 3 doctor doctor)
         (my-vector-ref2d shaves 3 barber doctor)
         (my-vector-ref2d shaves 3 barber mayor)
         (my-vector-ref2d shaves 3 mayor mayor)        
     )
   
   )
  )

(show-marginals (model)
                (list "barber shaves barber"
                      "doctor shaves doctor"
                      "barber shaves doctor"
                      "barber shaves mayor"
                      "mayor shaves mayor"
                      )
                #:num-samples 10000
                #:truncate-output 4
                ; #:skip-marginals? #t
                #:credible-interval 0.94
                ; #:show-stats? #t
                )


