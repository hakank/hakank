#| 

  Coin flip game in Racket.Gamble 

  Anne Ogborn:
  https://twitter.com/AnneOgborn/status/1560681513548759042
  """
  I offer a game-  flip a coin. Heads, I pay $1 and the game ends. Tails, we flip again- 
  if Heads, I pay $2 and game ends, Tails, we flip again, doubling each time.
  What would you pay me to play this game?
  """

  Comment by Richard Barrell:
  """
  Roughly 0.5*log2(your bankroll)
  Each round has an EV of $0.50 and the number of rounds is log2(the maximum possible 
  amount of money you can pay out)
  """
 
  (This is related to the St Petersburg "paradox"/lottery: 
   https://en.wikipedia.org/wiki/St._Petersburg_paradox)
 
  * With a limit of 2*12 Euro:
  var : game 1
  1: 1/2 (0.5)
  2: 1/4 (0.25)
  4: 1/8 (0.125)
  8: 1/16 (0.0625)
  16: 1/32 (0.03125)
  32: 1/64 (0.015625)
  64: 1/128 (0.0078125)
  128: 1/256 (0.00390625)
  256: 1/512 (0.001953125)
  512: 1/1024 (0.0009765625)
  1024: 1/2048 (0.00048828125)
  2048: 1/4096 (0.000244140625)
  4096: 1/8192 (0.0001220703125)
  8192: 1/16384 (6.103515625e-5)
  16384: 1/32768 (3.0517578125e-5)
  32768: 1/65536 (1.52587890625e-5)
  65536: 1/131072 (7.62939453125e-6)
  131072: 1/262144 (3.814697265625e-6)
  262144: 1/524288 (1.9073486328125e-6)
  524288: 1/1048576 (9.5367431640625e-7)
  1048576: 1/2097152 (4.76837158203125e-7)
  2097152: 1/4194304 (2.384185791015625e-7)
  4194304: 1/8388608 (1.1920928955078125e-7)
  8388608: 1/16777216 (5.960464477539063e-8)
  16777216: 1/33554432 (2.9802322387695313e-8)
  33554432: 1/67108864 (1.4901161193847656e-8)
  67108864: 1/134217728 (7.450580596923828e-9)
  134217728: 1/268435456 (3.725290298461914e-9)
  268435456: 1/536870912 (1.862645149230957e-9)
  536870912: 1/1073741824 (9.313225746154785e-10)
  1073741824: 1/2147483648 (4.656612873077393e-10)
  2147483648: 1/4294967296 (2.3283064365386963e-10)
  4294967296: 1/8589934592 (1.1641532182693481e-10)
  8589934592: 1/17179869184 (5.820766091346741e-11)
  17179869184: 1/34359738368 (2.9103830456733704e-11)
  34359738368: 1/68719476736 (1.4551915228366852e-11)
  68719476736: 1/137438953472 (7.275957614183426e-12)
  137438953472: 1/274877906944 (3.637978807091713e-12)
  274877906944: 1/549755813888 (1.8189894035458565e-12)
  549755813888: 1/1099511627776 (9.094947017729282e-13)
  1099511627776: 1/1099511627776 (9.094947017729282e-13)
  mean: 21 (21.0)

  var : log2ndiv2
  19.931568569324174: 1 (1.0)
  mean: 19.931568569324174

  var : est
  20.931568569324174: 1 (1.0)
  mean: 20.931568569324174

  var : p
  #f: 31/32 (0.96875)
  #t: 1/32 (0.03125)
  mean: 1/32 (0.03125)

  var : p2
  #f: 31/32 (0.96875)
  #t: 1/32 (0.03125)
  mean: 1/32 (0.03125)

  * With a limit of 2**20 (1048576)

  var : game 1
  1: 1/2 (0.5)
  2: 1/4 (0.25)
  4: 1/8 (0.125)
  8: 1/16 (0.0625)
  16: 1/32 (0.03125)
  32: 1/64 (0.015625)
  64: 1/128 (0.0078125)
  128: 1/256 (0.00390625)
  256: 1/512 (0.001953125)
  512: 1/1024 (0.0009765625)
  1024: 1/2048 (0.00048828125)
  2048: 1/4096 (0.000244140625)
  4096: 1/8192 (0.0001220703125)
  8192: 1/16384 (6.103515625e-5)
  16384: 1/32768 (3.0517578125e-5)
  32768: 1/65536 (1.52587890625e-5)
  65536: 1/131072 (7.62939453125e-6)
  131072: 1/262144 (3.814697265625e-6)
  262144: 1/524288 (1.9073486328125e-6)
  524288: 1/1048576 (9.5367431640625e-7)
  1048576: 1/1048576 (9.5367431640625e-7)
  mean: 11 (11.0)

  var : log2ndiv2
  10.0: 1 (1.0)
  mean: 10.0

  var : est
  11.0: 1 (1.0)
  mean: 11.0

  var : p
  #f: 15/16 (0.9375)
  #t: 1/16 (0.0625)
  mean: 1/16 (0.0625)

  var : p2
  #f: 15/16 (0.9375)
  #t: 1/16 (0.0625)
  mean: 1/16 (0.0625)


  p is the probability that we get at least the expected value (0.5*log2(n)).

  For larger n, it seems that (/ (log n 2) 2) underestimates the estimated value by 
  (exactly) 1.


  Testing for n (limited bankroll) as powers of 2

  (expt 2 n) (game 1)
  ------------------------------------------------------
        1 : 1
        2 : 1.5
        4 : 2
        8 : 2.5
       16 : 3
       32 : 3.5
       64 : 4
      128 : 4.5
      256 : 5
      512 : 5.5
     1024 : 6
     2048 : 6.5
     4096 : 7
     8192 : 7.5
    16384 : 8
    32768 : 8.5
    65536 : 9
   131072 : 9.5
   262144 : 10
   524288 : 10.5
  1048576 : 11

  Via JGAP (Symbolic Regressions), the estimated value for the powers of 2 results, is about
    (log (4 * n)) / (log(4))
  or:
    0.7213475205 ln(4.0 V1)

  Maple:
  > (log (4 * V1)) / (log(4));
                                                ln(4 V1)
                                                --------
                                                 ln(4)

  Mathematica:
  The estimate from Richard Barrell has the limit of:
    Limit[Log2[n] / 2, n -> Infinity]
    -> Infinity

    However, with some limit (bankroll) a:
    Limit[Log2[n] / 2, n -> a]
    -> Log[a]/Log[4]

  This estimation, i.e. (log (4 * n)) / (log(4)) seems to be exact for the powers of 2.
  Perhaps it would been simpler to adjust the original formula by subtracting 1:
    (- (/ (log n 2) 2) 1)

  Note: for values that are not powers of 2, this does not work as neat.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model n #:simple? [simple? #f])
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   ;; (define n (expt 10 130) ; we have some limit (bankroll)
   ;; (define n (expt 10 5)) ; we have some limit (bankroll)

   (defmem (game i)
     (let ([v (flip 1/2)])
       (if (or v (>= i n))
           i
           (game (* 2 i)))))
   
   
   ; Probability that we get at least the expected value
   ; (according to the 0.5*log2(n) formula)
   (define p (>= (game 1) (/ (log n 2) 2)))
   ; Checking the formula from JGAP (symbolic regression))
   ; This seems to give an exact closed for for (game 1)
   (define p2 (>= (game 1) (/ (log (* 4 n)) (log 4))))

   (if simple?
       (list (game 1)
             (/ (log n 2) 2)
             (- (game 1) (/ (log n 2) 2))
             (/ (log (* 4 n)) (log 4))
             (- (game 1) (/ (log (* 4 n)) (log 4)))
             )
       (list (game 1)
             (/ (log n 2) 2) 
             (/ (log (* 4 n)) (log 4)) 
             p
             p2
             )
       )

   
   )
)

(for ([n (list (expt 10 12) (expt 2 20))])
  (show "n" n)
  (show-marginals (model n)
                  (list  "game 1"
                         "log2ndiv2"
                         "est"
                         "p"
                         "p2"
                         ))
)

(displayln "\nChecking (game 1) vs (/ (log n 2) 2) vs (/ (log (* 4 n)) (log 4))")
(for ([n (range 1 21)])
  (show2 "** n" n (expt 2 n))
  (show-marginals (model (expt 2 n) #:simple? #t)
                  (list  "game 1"
                         "(/ (log n 2) 2)"
                         "diff1"
                         "(/ (log (* 4 n)) (log 4))"
                         "diff2"
                         )
                  #:skip-marginals? #t
                  )
)

