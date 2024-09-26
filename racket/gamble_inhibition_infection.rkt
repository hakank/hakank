#| 

  Inibition infection in Racket.Gamble 

  https://dtai.cs.kuleuven.be/problog/tutorial/basic/10_inhibitioneffects.html
  """
  Example 2: Social Network

  An infectious disease spreads through a population as follows: when- ever two people a
  re in regular contact with each other and one is infected, there is a probability of 0.6 
  of the infection spreading also to the other person. Given a set of initially infected 
  people and a graph of connections between individuals in the population, the goal is 
  to predict the spread of the disease.


   % ProbLog
   person(a).
   person(b).

   0.1::initialInf(X) :- person(X).
   0.1::contact(X,Y) :- person(X), person(Y).

   inf(X)      :- initialInf(X).
   0.6::inf(X) :- contact(X, Y), inf(Y).

   query(inf(_)). % inf(a): 0.1054 inf(b): 0.1054
   """


  var : inf 0
  #f: 0.8945991462530847
  #t: 0.10540085374691535
  mean: 0.10540085374691535

  var : inf 1
  #f: 0.8945991462530847
  #t: 0.10540085374691535
  mean: 0.10540085374691535

  var : inf all
  (#f #f): 0.809998460987534
  (#f #t): 0.08460068526555065
  (#t #f): 0.08460068526555065
  (#t #t): 0.020800168481364702


  This is a port of my WebPPL model inhibition_infection.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (enumerate
   #:limit 1e-05
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define n 2)
   (define people (range n))

   (defmem (initialInf p) (flip 0.1))
    
   (define (contact x y)
     (if (= x y) 
         #f
         (flip 0.1)))

   (defmem (inf p)
     (if (initialInf p)
         #t
         (let ([check (ormap (lambda (q)
                               (and (not (= p q)) (contact p q) (inf q))) people)])
           (if check
               (flip 0.6)
               #f))))
   
   ; (defmem (inf p) (inf1 p))

   (list (inf 0)
         (inf 1)
         ; (inf 2)
         ; (inf 3)
         ; (inf 4)
         (for/list ([p people]) (inf p))
    );
   )
)

(show-marginals (model)
                (list  "inf 0"
                       "inf 1"
                       ; "inf 2"
                       ; "inf 3"
                       ; "inf 4"
                       "inf all"
                       )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


