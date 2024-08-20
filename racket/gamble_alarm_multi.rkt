#| 

  Alarm problem (multi= in Racket Gamble.

  https://dtai.cs.kuleuven.be/problog/tutorial/basic/02_bayes.html
  """
  Since the random variables in the Bayesian network are all Boolean, we only need a single literal 
  in the head of the rules. We can extend the Bayesian network to have a multi-valued variable by 
  indicating the severity of the earthquake. The literal earthquake now has three possible values 
  none, mild, heavy instead of previously two (no or yes).
  """
  
  ProbLog model:
  """
  person(john).
  person(mary).
  
  0.7::burglary.
  0.01::earthquake(heavy); 0.19::earthquake(mild); 0.8::earthquake(none).
  
  0.90::alarm :-   burglary, earthquake(heavy).
  0.85::alarm :-   burglary, earthquake(mild).
  0.80::alarm :-   burglary, earthquake(none).
  0.10::alarm :- \+burglary, earthquake(mild).
  0.30::alarm :- \+burglary, earthquake(heavy).
  
  0.8::calls(X) :- alarm, person(X).
  0.1::calls(X) :- \+alarm, person(X).
  
  evidence(calls(john),true).
  evidence(calls(mary),true).
  
  query(burglary).
  query(earthquake(_)).
  """

  This is a port of my WebPPL model alarm_multi.wppl.

  Output of this Gamble model:

var : calls john
#t: 1.0
mean: 1.0

var : calls mary
#t: 1.0
mean: 1.0

var : alarm
#t: 0.9998840271937381
#f: 0.00011597280626194896
mean: 0.9998840271937381

var : burglary
#t: 0.9884222092292877
#f: 0.011577790770712318
mean: 0.9884222092292877

var : earthquake
none: 0.7805610903463548
mild: 0.2068949555721803
heavy: 0.012543954081464965

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (alarm-multi)
  
  (enumerate
   ; rejection-sampler
   
   (define burglary (flip 0.7))   
   (define earthquake (categorical-vw (vector "heavy" "mild" "none") (vector 0.01 0.19 0.8)))   
   (define alarm
     (cond
       [(and burglary (eq? earthquake "heavy")) (flip 0.9)]
       [(and burglary (eq? earthquake "mild"))  (flip 0.85)]
       [(and burglary (eq? earthquake "none"))  (flip 0.80)]
       [(and (not burglary) (eq? earthquake "heavy")) (flip 0.30)]
       [(and (not burglary) (eq? earthquake "mild")) (flip 0.10)]
       [else #f]))
   
   ;; Note: memoized function
   (define calls
     (mem (lambda (p) (if alarm
                          (flip 0.8)
                          (flip 0.01)))))
   

   ;; Both John and Mary calls
   (observe/fail (eq? (calls "john") #t))
   (observe/fail (eq? (calls "mary") #t))    
   
   (list (calls "john")
         (calls "mary")
         alarm
         burglary
         earthquake)

   )
  )

(show-marginals (alarm-multi)
                (list 
                 "calls john"
                 "calls mary"
                 "alarm"
                 "burglary"
                 "earthquake")
                )

                
                
