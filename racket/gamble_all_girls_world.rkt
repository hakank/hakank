#| 

  All Girls World puzzle in Racket Gamble.

  From https://brainstellar.com/puzzles/probability/6
  """
  All Girls World?

  In a world where everyone wants a girl child, each family continues having 
  babies till they have a girl. What do you think will the boy-to-girl ratio 
  be eventually?

  Assuming probability of having a boy or a girl is the same and there is no 
  other gender at the time of birth.

  
  Answer:
  Suppose there are N couples. First time, N/2 girls and N/2 boys are born. N/2 
  couples retire, and rest half try for another child.

  Next time, N/4 couples give birth to N/4 girls and N/4 boys. Thus, even in the 
  second iteration, the ratio is 1:1. It can now be seen that this ratio will 
  always remain the same, no matter how many times people try to give birth to a 
  favored gender.
  """

  Note: I added a limit of the maximum number of children in a family (20 for
        the current model) since otherwise it might go on indefinitely. 

var : len
1: 1/2 (0.5)
2: 1/4 (0.25)
3: 1/8 (0.125)
4: 1/16 (0.0625)
5: 1/32 (0.03125)
6: 1/64 (0.015625)
7: 1/128 (0.0078125)
8: 1/256 (0.00390625)
9: 1/512 (0.001953125)
10: 1/1024 (0.0009765625)
11: 1/2048 (0.00048828125)
12: 1/4096 (0.000244140625)
13: 1/8192 (0.0001220703125)
14: 1/16384 (6.103515625e-5)
15: 1/32768 (3.0517578125e-5)
16: 1/65536 (1.52587890625e-5)
17: 1/131072 (7.62939453125e-6)
18: 1/262144 (3.814697265625e-6)
19: 1/524288 (1.9073486328125e-6)
20: 1/1048576 (9.5367431640625e-7)
21: 1/2097152 (4.76837158203125e-7)
22: 1/2097152 (4.76837158203125e-7)
mean: 4194303/2097152 (1.9999995231628418)

var : ratio
0: 1/2 (0.5)
1: 1/4 (0.25)
2: 1/8 (0.125)
3: 1/16 (0.0625)
4: 1/32 (0.03125)
5: 1/64 (0.015625)
6: 1/128 (0.0078125)
7: 1/256 (0.00390625)
8: 1/512 (0.001953125)
9: 1/1024 (0.0009765625)
10: 1/2048 (0.00048828125)
11: 1/4096 (0.000244140625)
12: 1/8192 (0.0001220703125)
13: 1/16384 (6.103515625e-5)
14: 1/32768 (3.0517578125e-5)
15: 1/65536 (1.52587890625e-5)
16: 1/131072 (7.62939453125e-6)
17: 1/262144 (3.814697265625e-6)
18: 1/524288 (1.9073486328125e-6)
19: 1/1048576 (9.5367431640625e-7)
20: 1/2097152 (4.76837158203125e-7)
21: 1/2097152 (4.76837158203125e-7)
mean: 2097151/2097152 (0.9999995231628418)

  The ration is thus 1:~0.9999995231628418 (with a limit of 20 children per family)

  For a limit of 100 the ratio is almost 1:
  2535301200456458802993406410751/2535301200456458802993406410752 (1.0)


  Cf the geometric distribution:
> (map (lambda (i) (- 1 (dist-cdf (geometric-dist 0.5) i))) (range 10))
'(0.5
  0.25
  0.125
  0.0625
  0.03125
  0.015625
  0.0078125
  0.00390625
  0.001953125
  0.0009765625)

> (apply + (map (lambda (i) (- 1 (dist-cdf (geometric-dist 1/2) i))) (range 10)))
0.9990234375

> (apply + (map (lambda (i) (- 1 (dist-cdf (geometric-dist 1/2) i))) (range 100)))
0.9999999999999999


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (all-girls-world)
  
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define limit 20)
   (define (gender) (uniform-draw '("boy" "girl")) )

   (define (family lst)
     (let* ([c (gender)]
            [lst2 (append lst (list c))])
       (if (or (eq? c "girl") (> (length lst) limit))
           lst2
           (family lst2)
           )))

   (define this-family (family '()))
   (define len (length this-family))
   (define ratio (sub1 len))
   
   (list len ratio)
   
   )
  )


(time (show-marginals (all-girls-world)
                (list "len" "ratio")
                #:num-samples 100000
                ; #:truncate-output 10
                ; #:show-stats? #t 
                ; #:credible-interval 0.84
                ))
