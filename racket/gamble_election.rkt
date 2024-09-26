#| 

  Election in Racket.Gamble 

  This is a port of the SPPL model election.pynb
  """
  from sppl.sym_util import binspace
  n = 4000
  param ~= randint(low=250, high=350)
  switch (param) cases (b in range(250, 350)):
    p ~= beta(a=277, b=b)
  switch (p) cases (x in binspace(0, 1, 20)):
    votes ~= binom(n=n, p=(x.left + x.right)/2)
  win ~= votes > 0.5*n
  """

  var : param
  mean: 299.3770000000289
  Credible interval (0.94): 254..347

  var : p
  mean: 0.4816859874393472
  Credible interval (0.94): 0.425529616395625..0.5414114200049521

  var : votes
  mean: 1926.9480000001786
  Credible interval (0.94): 1680..2161

  var : win
  mean: 0.2954000000000238
  Credible interval (0.94): 0..1

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

;
; First model, straightward port of the model using set!
; Enumerate gives strange results (as usual with set! models)
;
(define (model)
  (; enumerate 
   ; rejection-sampler
   importance-sampler
   ; mh-sampler ; #:transition (slice)

   
   (define n 4000)
   (define param (+ 250 (random-integer 100)))
   (define p (beta 277 param))
   (define votes (binomial n p))

   (define win (> votes (* 0.5 n)))

   (list param
         p
         votes
         win
    )

   )
)

(show-marginals (model)
                (list  "param"
                       "p"
                       "votes"
                       "win"
                       )
                #:num-samples 10000
                #:truncate-output 5
                #:skip-marginals? #t
                ; #:show-stats? #t
                #:credible-interval 0.94
                ; #:credible-interval2 0.94                
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )
