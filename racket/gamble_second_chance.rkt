#| 

  Second Chance in Racket.Gamble 

  https://brainstellar.com/puzzles/probability/114
  """
  Roll a die, and you get paid what the dice shows. But if you 
  want, you can request a second chance & roll the die again; get paid what 
  the second roll shows instead of the first. What is the expected value?
  """
  
  If we roll again if first roll is below or equal a limit then the best limit is
  3 with an expected outcome of 17/4 (4.25)

limit: 1
var : roll
2: 7/36 (0.19444444444444445)
3: 7/36 (0.19444444444444445)
4: 7/36 (0.19444444444444445)
5: 7/36 (0.19444444444444445)
6: 7/36 (0.19444444444444445)
1: 1/36 (0.027777777777777776)
mean: 47/12 (3.9166666666666665)


limit: 2
var : roll
3: 2/9 (0.2222222222222222)
4: 2/9 (0.2222222222222222)
5: 2/9 (0.2222222222222222)
6: 2/9 (0.2222222222222222)
1: 1/18 (0.05555555555555555)
2: 1/18 (0.05555555555555555)
mean: 25/6 (4.166666666666667)


limit: 3
var : roll
4: 1/4 (0.25)
5: 1/4 (0.25)
6: 1/4 (0.25)
1: 1/12 (0.08333333333333333)
2: 1/12 (0.08333333333333333)
3: 1/12 (0.08333333333333333)
mean: 17/4 (4.25)


limit: 4
var : roll
5: 5/18 (0.2777777777777778)
6: 5/18 (0.2777777777777778)
1: 1/9 (0.1111111111111111)
2: 1/9 (0.1111111111111111)
3: 1/9 (0.1111111111111111)
4: 1/9 (0.1111111111111111)
mean: 25/6 (4.166666666666667)


limit: 5
var : roll
6: 11/36 (0.3055555555555556)
1: 5/36 (0.1388888888888889)
2: 5/36 (0.1388888888888889)
3: 5/36 (0.1388888888888889)
4: 5/36 (0.1388888888888889)
5: 5/36 (0.1388888888888889)
mean: 47/12 (3.9166666666666665)


limit: 6
var : roll
1: 1/6 (0.16666666666666666)
2: 1/6 (0.16666666666666666)
3: 1/6 (0.16666666666666666)
4: 1/6 (0.16666666666666666)
5: 1/6 (0.16666666666666666)
6: 1/6 (0.16666666666666666)
mean: 7/2 (3.5)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model limit)
  (enumerate

   (define n 6)
   (define (roll-dice i) (add1 (random-integer n)))
   (define roll1 (roll-dice 1))
   (define roll (if (<= roll1 limit) (roll-dice 2) roll1))

   (list roll))
)

(for ([limit (range 1 7)])
  (newline)
  (displayln (format "limit: ~a" limit))
  (show-marginals (model limit)
                  (list  "roll")))

