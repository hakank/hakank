#| 

  Handedness in Racket Gamble.

  From infer.net test/Tests/BlogTests.cs
  """
  probRightHandedActual: Beta(7.72,3.08)[mean=0.7148]
  """

  Note: It's a little confusing since the Infer.net program 
        includes different experiments.

  var : betaA
  mean: 6.438251538012128

  var : betaB
  mean: 2.670881791914906

  var : prob
  mean: 0.8159291369746818

  This is a port of my WebPPL model handedness.wppl.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (handedness)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

    (define student-data '(#f #t #t #t #t #t #t #t #f #f))
    (define lecturer-data '(#f #t #t #t #t #t #t #t #t #t))
    
    ; (define data student-data)
    (define data lecturer-data)
    
    (define len (length data))

    (define betaA (uniform 0 len))
    (define betaB (uniform 0 len))
    (define prob (beta betaA betaB))

    ; (displayln (list "len" len "betaA" betaA "betaB" betaB "prob" prob))
    
    ;; Note: This is from one experiment. 
    ;; (define prob (beta 0.72  0.08))

    ;; And this is from another experiment
    ; (define probExpected (beta 7.72 3.08))
    ;; (define prob (beta 7.72 3.08))
    
    (define (isRightHanded student)
      (flip-dist prob)
      ; (flip 0.9)
    )

    (for ([i (range len)])
      (observe-sample (isRightHanded i) (list-ref data i)))
    
    (list betaA
          betaB
          prob
          ; probExpected
          )

   )
  )

(show-marginals (handedness)
                (list "betaA"
                      "betaB"
                      "prob"
                      ; "probExpected"
                      )
                #:num-samples 10000
                #:truncate-output 4
                #:skip-marginals? #t
                )

