#| 

  Innocent Monkey in Racket.Gamble 

  https://brainstellar.com/puzzles/probability/115
  """
  A very innocent monkey throws a fair die. The monkey will eat as many 
  bananas as are shown on the die, from 1 to 5. But if the die shows '6', 
  the monkey will eat 5 bananas and throw the die again. This may continue 
  indefinitely. What is the expected number of bananas the monkey will eat?

  Answer: 4
  """

  * enumerate #:limit 1e-15  (1.7s)
  var : len
  mean: 1462463376025170/1218719480020991 (1.1999999999999842)

  var : total
  mean: 14624633760251603/3656158440062973 (3.999999999999921)

  * importance-sampler (1000000 samples, 7.1s)
  var : len
  mean: 1.1999719999999998

  var : total
  mean: 4.000689000000002

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (enumerate #:limit 1e-15
   ; importance-sampler

   (define n 6)
   
   (define (f a)
     (let ([r (add1 (random-integer n))])
       (if (<= r 5)
           (append a (list r))
           (f (append a (list 5))))))

   (define a (f '()))
   (define len (length a))
   (define total (sum a))
   
   (list len
         total)

   )
)

(show-marginals (model)
                (list  "len" 
                       "total")
                #:num-samples 1000000
                #:skip-marginals? #t
                )


