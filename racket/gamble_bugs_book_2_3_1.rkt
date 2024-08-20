#| 

  BUGS book, 2.3.1 in Racket Gamble.
  Page 17

var : x
17.28874820962826: 0.0009999999999999994
7.945568033185668: 0.0009999999999999994
17.731276410306656: 0.0009999999999999994
12.247194257163025: 0.0009999999999999994
8.474220219247377: 0.0009999999999999994
...
7.42551352059388: 0.0009999999999999994
7.639563057142556: 0.0009999999999999994
12.600889858216838: 0.0009999999999999994
12.110811657132052: 0.0009999999999999994
7.730340087706241: 0.0009999999999999994
mean: 10.075513061095283

Credible interval (0.84): 6.733193090573417..13.514367119877303

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

(require racket)
(require "gamble_utils.rkt")


(define (bugs-book-2-1-2)
  (; enumerate
   rejection-sampler

   ; degress mode scale
   (define x (t-dist 4 10 2))

   (list (sample x))
   )
  )

(show-marginals (bugs-book-2-1-2)
                (list "x")
                #:truncate-output 5
                )

(let ([sample (map (lambda (v) (car v)) (repeat (bugs-book-2-1-2) 10000))])
  (show-stats sample)  
  (show-credible-interval sample 0.84)
  )
