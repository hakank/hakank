#| 

  Simpson's paradox in Racket Gamble.


  http://cplint.eu/example/inference/simpson.swinb
  """
  From "Pearl, Judea. Causality. Cambridge university press, 2009"

  Simpson's paradox ... refers to the phenomenon whereby an event C increases the probability 
  of E in a given population p and, at the same time, decreases the probability 
  of E in every subpopulation of p. In other words, if F and ¬F are two complementary 
  properties describing two subpopulations, we might well encounter the inequalities
    P(E|C)>P(E|¬C)
    P(E|C,F)<P(E|¬C,F)
    P(E|C,¬F)<P(E|¬C,¬F)
   ... 
  For example, if we associate C (connoting cause) with taking a certain drug, E (connoting effect) with 
  recovery, and F with being a female, then ... the drug seems to be harmful to both males and females yet 
  beneficial to the population as a whole. 

  ...

  % cplint code:

  female:0.5.

  recovery:0.6:- drug,\+ female.
  recovery:0.7:- \+ drug,\+ female.

  recovery:0.2:- drug,female.
  recovery:0.3:- \+ drug,female.

  drug:30/40:- \+ female.
  drug:10/40:-female.

  If we query for the conditional probabilities of recovery given treatment
  on the whole population and on the two subpopulations, we get the results
  in the tables above:

  ?- prob(recovery,drug,P).
  P = 0.49999999999999994

  ?- prob(recovery,\+ drug,P).
  P = 0.40000000000000013

  ?- prob(recovery,(drug,female),P).
  P = 0.2

  ?- prob(recovery,(\+drug,female),P).
  P = 0.3000000000000001

  ?- prob(recovery,(drug,\+female),P).
  P = 0.6

  ?- prob(recovery,(\+ drug,\+female),P).
  P = 0.7000000000000002
  """


  The tests in the cplint model:

  * P(recovery | drug)
  var : recovery
  #f: 0.5
  #t: 0.5
  mean: 0.5

  * P(recovery | not drug)
  var : recovery
  #f: 0.6000000000000001
  #t: 0.4
  mean: 0.4

  * P(recovery | drug and female)

  var : recovery
  #f: 0.8
  #t: 0.20000000000000004
  mean: 0.20000000000000004

  * P(recovery | not drug and female)

  var : recovery
  #f: 0.7
  #t: 0.30000000000000004
  mean: 0.30000000000000004

  * P(recovery | drug and not female)

  var : recovery
  #t: 0.6000000000000001
  #f: 0.4
  mean: 0.6000000000000001

  * P(recovery | not  drug and not female)

  var : recovery
  #t: 0.7
  #f: 0.30000000000000004
  mean: 0.7

  * (P(drug,female | recovery)
  var : drug
  #t: 0.5555555555555556
  #f: 0.44444444444444453
  mean: 0.5555555555555556

  var : female
  #f: 0.6944444444444444
  #t: 0.30555555555555564
  mean: 0.30555555555555564

  * (P(drug,female | not recovery)

  var : drug
  #f: 0.5454545454545454
  #t: 0.4545454545454546
  mean: 0.4545454545454546

  var : female
  #t: 0.6590909090909091
  #f: 0.34090909090909094
  mean: 0.6590909090909091


  Model 2 does a systematic check for all combinations (where at least one variable is unobserved).

  This is a port of my WebPPL model simpson.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model)
  
  (enumerate 
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define female (flip 0.5))

   (define drug (if female (flip 10/40) (flip 30/40)))

   (define recovery
     (cond
       [(and drug female) (flip 0.2)]
       [(and drug (not female)) (flip 0.6)]
       [(and (not drug) female) (flip 0.3)]
       [(and (not drug) (not female)) (flip 0.7)]
       [else #f]))
   
   (observe/fail drug);
   ; (observe/fail (not drug));
   ; (observe/fail (and drug female))
   ; (observe/fail (and (not drug) female))
   ; (observe/fail (and drug (not female)))
   ; (observe/fail (and (not drug) (not female)))
   ; (observe/fail recovery)
   ; (observe/fail (not recovery))
      
   (list drug
         female
         recovery
        )

   )
  )

(show-marginals (model)
                (list "drug"
                      "female"
                      "recovery"                         
                      )
                #:num-samples 10000
                )


(define (model2 drug_val female_val recovery_val)
  
  (enumerate 
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define female (flip 0.5))

   (define drug (if female (flip 10/40) (flip 30/40)))

   (define recovery
     (cond
       [(and drug female) (flip 0.2)]
       [(and drug (not female)) (flip 0.6)]
       [(and (not drug) female) (flip 0.3)]
       [(and (not drug) (not female)) (flip 0.7)]
       [else #f]))
   
   ; (observe/fail drug);
   ; (observe/fail (not drug));
   ; (observe/fail female);
   ; (observe/fail (not female))
   ; (observe/fail recovery)
   ; (observe/fail (not recovery))
   
   (when (not (eq? drug_val "na"))
     (observe/fail (eq? drug drug_val)))
   
   (when (not (eq? female_val "na"))
     (observe/fail (eq? female female_val)))
   
   (when (not (eq? recovery_val "na"))
       (observe/fail (eq? recovery recovery_val)))
   
   (list drug
         female
         recovery
        )

   )
  )

#|
(for* ([drug_val '("na" #t #f)]
       [female_val '("na" #t #f)]
       [recovery_val '("na" #t #f)])
  ; At least one is unobserved
  (when (or (eq? drug_val "na") (eq? female_val "na") (eq? recovery_val "na"))
    (show2 "drug:" drug_val "female:" female_val "recovery:" recovery_val)
    (show-marginals (model2 drug_val female_val recovery_val)
                    (list "drug"
                          "female"
                          "recovery"                         
                          )
                    #:num-samples 10000
                    )
    )
)
|#
