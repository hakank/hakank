#| 

  The Euro coin problem Racket Gamble.

  From Think Bayes, page 33ff
  """
  A statistical statement appeared in "The Guardian" on Friday January 4, 2002:
      When spun on edge 250 times, a Belgian one-euro coin
      came up heads 140 times and tails 110. 'It looks very
      suspicious to me,' said Barry Blight, a statistics lecturer
      at the London School of Economics. 'If the coin were
      unbiased, the chance of getting a result as extreme as
      that would be less than 7%.'

  But do these data give evidence that the coin is biased rather than fair?
  """
 
  Below are two models, the first one is slow and convuluted, but
  the second model is faster and neater.

  This is a port of my WebPPL model euro_coin_problem.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

#|
  Slow model (18.3s)


Model 1
var : prob
0.5253178383583735: 0.0009999999999999994
0.5857169313305104: 0.0009999999999999994
0.5357803380347701: 0.0009999999999999994
0.6146413944751528: 0.0009999999999999994
0.6025313631575081: 0.0009999999999999994
...
0.49150751052208996: 0.0009999999999999994
0.6164900868376161: 0.0009999999999999994
0.5286516171174994: 0.0009999999999999994
0.5908789442118171: 0.0009999999999999994
0.5377842919283699: 0.0009999999999999994
mean: 0.561051203332307

var : prob < 0.5
#f: 0.9750000000000008
#t: 0.024999999999999998
mean: 0.024999999999999998

var : prob > 0.5
#t: 0.9750000000000008
#f: 0.024999999999999998
mean: 0.9750000000000008

|#
(define (euro-coin-problem-1)

  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 250)
   
   (define binom (binomial n (/ 140 n)))
    
   ;; Probability of throwing head
   (define prob (beta 2 2))
   (define coin (lambda (i)
                            (bernoulli prob)))
   
   (define sum250 (for/sum ([i (range n)]) (coin i)))
   
   (observe/fail (= sum250 140))

   (list prob
         (< prob 0.5)
         (> prob 0.5)
    )
   )
  )

;; (displayln "Model 1")
;; (show-marginals (euro-coin-problem-1)
;;                   (list "prob"
;;                         "prob < 0.5"
;;                         "prob > 0.5"
;;                         )
;;                   #:num-samples 1000
;;                   #:truncate-output 5
;;                   ; #:skip-marginals? #t
;;                   )



#|

  Faster and simpler model: 2.8s


Model 2
var : prob
0.5600775057901465: 0.000864297719365134
0.5598598313047005: 0.0008642917391615758
0.5598257806068405: 0.0008642870457649119
0.5602044720424992: 0.0008642820206582708
0.5602634166868673: 0.000864269926976713
...
0.00830771945906499: 5.488633647145647e-221
0.0072416963121243105: 2.7604365505213846e-229
0.9987762296397136: 9.618038963344175e-250
0.004919923804928562: 1.1197024842532574e-252
0.004767646835750148: 1.3957600699642164e-254
mean: 0.558617942221096

var : prob < 0.5
#f: 0.9685861915690971
#t: 0.03141380843090531
mean: 0.03141380843090531

var : prob > 0.5
#t: 0.9685861915690971
#f: 0.03141380843090531
mean: 0.9685861915690971


|#
(define (euro-coin-problem-2)

  (; enumerate ; can not enumerate beta
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 250)

   ;; Probability of throwing head
   (define prob (beta 2 2))
   (define heads (binomial-dist n prob))
    
   (observe-sample heads 140)

   (list prob
         (< prob 0.5)
         (> prob 0.5)
    )
   )
  )

(displayln "Model 2")
(show-marginals (euro-coin-problem-2)
                  (list "prob"
                        "prob < 0.5"
                        "prob > 0.5"
                        )
                  #:num-samples 10000
                  #:truncate-output 5
                  ; #:skip-marginals? #t
                  )
