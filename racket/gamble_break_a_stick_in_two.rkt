#| 

  Break a stick in two in Racket.Gamble 

  From Julia Simon "Resample Stats", page 42:
  """
  In a book of puzzles about probability (Mosteller, 1965/1987,#42), 
  this problem appears: “If a stick is broken in two at random, what is 
  the average length of the smaller piece?”
  ...
  Yet a rephrasing of the problem reveals its tie to the concept of probability,
  to wit: What is the probability that the smaller piece will be (say) more than 
  half the length of the larger piece? Or, what is the probability distribution of the 
  sizes of the shorter piece?
  """

  var : min
  0.0027698223889160573: 0.00010000000000000938
  0.40257428370775905: 0.00010000000000000938
  0.12389747956084907: 0.00010000000000000938
  0.05271675739555749: 0.00010000000000000938
  0.2565860674646454: 0.00010000000000000938
  ...
  0.1807340648008244: 0.00010000000000000938
  0.35099835344768543: 0.00010000000000000938
  0.12030859245550518: 0.00010000000000000938
  0.10882273587284808: 0.00010000000000000938
  0.3472907129294397: 0.00010000000000000938
  mean: 0.25043775954059194
  Credible interval (0.84): 0.04780442848413278..0.4633117556499422
  Percentiles:
  (0.01 0.004659501130035202)
  (0.025 0.012603439535367023)
  (0.1 0.050827374349370104)
  (0.05 0.025089532886310995)
  (0.25 0.12466288705586459)
  (0.5 0.24961225104503063)
  (0.75 0.37160033413508664)
  (0.84 0.4170718439274801)
  (0.9 0.4478698042586724)
  (0.95 0.47274475622244866)
  (0.975 0.4857912371038873)
  (0.99 0.4944619687386066)
  (0.999 0.4995913463446778)
  Histogram:
  0    : 1  
  0.011: 230
  0.023: 219
  0.034: 231
  0.045: 204
  0.057: 257
  0.068: 228
  0.08 : 227
  0.091: 245
  0.102: 222
  0.114: 228
  0.125: 217
  0.136: 204
  0.148: 200
  0.159: 221
  0.17 : 241
  0.182: 240
  0.193: 235
  0.205: 219
  0.216: 237
  0.227: 241
  0.239: 229
  0.25 : 228
  0.261: 233
  0.273: 229
  0.284: 221
  0.295: 271
  0.307: 237
  0.318: 225
  0.33 : 228
  0.341: 238
  0.352: 215
  0.364: 241
  0.375: 245
  0.386: 215
  0.398: 233
  0.409: 221
  0.42 : 204
  0.432: 232
  0.443: 232
  0.455: 207
  0.466: 251
  0.477: 204
  0.489: 218

  var : p
  #f: 0.6628999999999834
  #t: 0.3371000000000192
  mean: 0.3371000000000192

  This is a port of my WebPPL model break_a_stick_in_two.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   ;; Let's assume that the stick is 1 unit long
   (define u1 (uniform 0 1))
   (define u2 (- 1 u1))
   (define min-u (min u1 u2))
   (define max-u (max u1 u2))   

   ;; What is the probability that the smaller piece will be (say) more than 
   ;; half the length of the larger piece?
   (define p (> min-u (/ max-u 2)))
    
   (list min-u
         p
         )

   )
)

(show-marginals (model)
                (list  "min"
                       "p"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                #:credible-interval 0.84
                #:show-histogram? #t
                #:show-percentiles? #t
                )


