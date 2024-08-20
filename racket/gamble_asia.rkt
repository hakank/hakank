#| 

  Asia problem in Racket Gamble.

  This is a port of my WebPPL model asia.wppl which is a port of the
  Church model https://github.com/stuhlmueller/jschurch/blob/master/tests/asia.church

var : smoker
#t: 0.6700973677390694
#f: 0.3299026322609306
mean: 0.6700973677390694

var : tb
#f: 0.9592758505793935
#t: 0.040724149420606454
mean: 0.040724149420606454

var : cancer
#f: 0.9222538965606605
#t: 0.07774610343933955
mean: 0.07774610343933955

var : tb_or_cancer
#f: 0.885268964495946
#t: 0.11473103550405395
mean: 0.11473103550405395

var : bronchitis
#t: 0.9999999999999999
mean: 0.9999999999999999

var : xray
#f: 0.8433001369812299
#t: 0.1566998630187702
mean: 0.1566998630187702

var : dyspnoea
#t: 0.9999999999999999
mean: 0.9999999999999999


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

   (define smoker (flip 0.5))
   (define tb (flip (if smoker 0.05 0.01)))
   (define cancer (flip (if smoker 0.1 0.01)))
   (define tb_or_cancer (or tb cancer))
   (define bronchitis (flip (if smoker 0.6 0.3)))
   (define xray (flip (if tb_or_cancer 0.98 0.05)))
   (define dyspnoea (flip
                     (if tb_or_cancer
                         (if bronchitis 0.9 0.7)
                         (if bronchitis 0.8 0.1))))
    
   (observe/fail bronchitis)
   (observe/fail dyspnoea)

   (list
        smoker
        tb
        cancer
        tb_or_cancer
        bronchitis
        xray
        dyspnoea
    )
  

   )
  )

(show-marginals (model)
                  (list "smoker"
                        "tb"
                        "cancer"
                        "tb_or_cancer"
                        "bronchitis"
                        "xray"
                        "dyspnoea"
                        )
                  #:num-samples 10000
                  ; #:truncate-output 1
                  ; #:skip-marginals? #t
                  ; #:show-histogram? #t
                  )
