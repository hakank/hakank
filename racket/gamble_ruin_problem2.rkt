#| 

  Ruin problem in Racket.Gamble 


  Compared to gamble_ruin_problem.rkt this version 
  use a recursive approach with arrays instead.

  The lists (a) that ends with a 0 indicates ruin.
  Note: We have to have a limit since otherwise the sequence might go to 
  infinity...

  * Limit of 50

  var : len
  2: 0.499400000000428
  4: 0.12493000000010707
  50: 0.11466000000011209
  6: 0.06303000000005402
  8: 0.03910000000003351
  ...
  40: 0.0032600000000028036
  42: 0.003020000000002595
  46: 0.002610000000002238
  44: 0.002580000000002212
  48: 0.002250000000001925
  mean: 11.236740000010311

  var : ruin
  #t: 0.8876400000003327
  #f: 0.11236000000010979
  mean: 0.8876400000003327

  * With a limit of 100

  var : len
  mean: 15.908340000014492

  var : ruin
  mean: 0.9208600000001815

  * With a limit of 500

  var : len
  mean: 36.009080000000004

  var : ruin
  mean: 0.96401



  This is a port of my WebPPL model ruin_problem2.wppl

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

   (define limit 50) 
   (define coins '("head" "tail"))
   (define start 1)
   (define win 1)
   (define loose 1)
   
   (define (draw arr)
     (let ([len (length arr)])
       (cond [(= len 0) (draw (list start))]
             [(or (= (last arr) 0) (>= len limit)) arr]
             [else
              (let ([lastVal (last arr)]
                    [c (uniform-draw coins)])
                (cond
                  [(eq? c "head") (draw (append arr (list (+ lastVal win))))]
                  [(<= (- lastVal loose) 0) (append arr (list 0))]
                  [else (draw (append arr (list (- lastVal loose))))]))])))
  
   (define a (draw (list start)))
   (define len (length a))
    
   (list ; a
         len
         (= (last a) 0)
    )
   )
)

(show-marginals (model)
                (list  ; "a"
                       "len"
                       "ruin"
                       )
                #:num-samples 100000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


