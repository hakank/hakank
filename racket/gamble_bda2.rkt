#| 

  BDA2  in Racket Gamble.

  From the WebPPL model https://github.com/probmods/ppaml2016/blob/gh-pages/chapters/5-data.md
  """
  Posterior prediction and model checking

  One important use of posterior predictive distributions is to examine the descriptive 
  adequacy of a model. The posterior predictive can be viewed as a set of predictions about 
  what data the model expects to see, based on the posterior distribution over parameters. 
  If these predictions do not match the data already seen, the model is descriptively inadequate.

  Let's say we ran 2 experiments that we believe are conceptually the same (e.g., asking 
  10 people whether or not they would vote for "Hillary Clinton or Donald Trump" and asking 
  a separate group of 10 people if they would vote for "Donald Trump or Hillary Clinton"). 
  Suppose we observed the following data from those 2 experiments: k1=0; k2=10.
  """

var : p
mean: 0.4993368131179026
Credible interval (0.84): 0.3750619855739393..0.6235338998248454

var : prior_p
mean: 0.5000214369988472
Credible interval (0.84): 0.035057270734550505..0.8647993530329005

var : posteriorPredictive1
mean: 4.993989626087702
Credible interval (0.84): 0..9

var : posteriorPredictive2
mean: 5.028996472292504
Credible interval (0.84): 0..9



  This is a port of my Julia model bda2.jl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model)
  
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n1 10)
   (define n2 10)

   ; Sample rate from Uniform distribution
   (define p (uniform 0 1))
    
   (define k1 (binomial-dist n1 p))
   (define k2 (binomial-dist n2 p))

   (observe-sample k1 0)
   (observe-sample k2 10)   
   
   ; sample from binomial with updated p
   (define prior_p (uniform 0 1))
   (define posteriorPredictive1 (binomial n1 prior_p))
   (define posteriorPredictive2 (binomial n2 prior_p))

   (list p
         prior_p
         posteriorPredictive1
         posteriorPredictive2
         )
   
   )
  )

(show-marginals (model)
                (list  "p"
                      "prior_p"
                      "posteriorPredictive1"
                      "posteriorPredictive2"
                      )
                #:num-samples 10000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                #:credible-interval 0.84
                ; #:show-histogram #t
                ; #:show-percentile #t
                )

