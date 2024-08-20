#| 
  Dice problem in Gamble.

  From Think Bayes, page 21.
  """
  Suppose I have a box of dice that contains a 4-sided die, a 6-sided die, an
  8-sided die, a 12-sided die, and a 20-sided die. If you have ever played
  Dungeons & Dragons, you know what I am talking about.

  Suppose I select a die from the box at random, roll it, and get a 6. What is
  the probability that I rolled each die?

   ...
   
  What if we roll a few more times and get 6, 8, 7, 7, 5, and 4?
  """

  a) We observe 6

     enumerate:
     (6 : 20/51 (0.39215686274509803))
     (8 : 5/17 (0.29411764705882354))
     (12 : 10/51 (0.19607843137254902))
     (20 : 2/17 (0.11764705882352941))
     (mean: 9.411764705882353)

     sampler
     (6 : 0.3955417832866853)
     (8 : 0.30607756897241106)
     (12 : 0.18882447021191523)
     (20 : 0.1095561775289884)
     (mean: 9.278988404638145)

   b) We observe 6, 8, 7, 7, 5, 4 

      enumerate 
      (8 : 11390625/12437281 (0.91584527196901))
      (12 : 1000000/12437281 (0.08040342579700499))
      (20 : 46656/12437281 (0.0037513022339850646))
      (mean: 8.36662932999584)

      sampler
      (8 : 0.9261221633509947)
      (12 : 0.0729781065680296)
      (20 : 0.0008997300809757073)
      (mean: 8.302809157252824)

   c) Extra: We observe 1 1 1 1 1 1 1 1 1
      enumerate:

      (4 : 729000000000/749424201373 (0.9727468083689033))
      (6 : 512000000000/20234453437071 (0.025303376816790044))
      (8 : 1423828125/749424201373 (0.0018998961100955142))
      (12 : 1000000000/20234453437071 (4.9420657845293055e-5))
      (20 : 373248/749424201373 (4.980463658848785e-7))
      (mean: 4.058609672078578)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

;;; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define observed1 '(6))
(define observed2 '(6 8 7 7 5 4))

(define (dice) (uniform-draw '(4 6 8 12 20)))

(define (enumerate-run observations)
  (displayln (list "observations" observations))
  (enumerate
   (define d (dice))
   (for ([obs observations])
     (define throw (add1 (random-integer d)))
     (observe/fail (= throw obs))
     )
   d)
  )

(displayln "enumerate-run")
(time (show-model (enumerate-run observed1)))
(newline)
(time (show-model (enumerate-run observed2)))
(newline)
(time (show-model (enumerate-run '(1 1 1 1 1 1 1 1 1))))
(newline)


(define (sampler-run observations)
  (displayln (list "observations" observations))  
  (; rejection-sampler ; too slow
   ; importance-sampler ; error
   mh-sampler ; warning: adaptive-drift-proposal: scale decreased out of range
   
   (define d (dice))
   (for ([obs observations])
     (define throw (add1 (random-integer d)))
     (observe/fail (= throw obs))
     )
   d)
  )

;; (displayln "sampler-run")
;; (time (show-freq (repeat (sampler-run observed1) 10000)))
;; (newline)
;; ; This is quite slow
;; ; (time (show-freq (repeat (sampler-run observed2) 10000)))
;; (newline)
