#| 

  Brian ate pizza last night in Racket.Gamble 

  CUFP 2013: Avi Pfeffer: Functional Probabilistic Programming
  From https://www.youtube.com/watch?v=U67guma2H6s
  @3.56
  Brian ate pizza last night. Is he a programmer or a student?

  * No observation

  var : student
  #t: 7/10 (0.7)
  #f: 3/10 (0.3)
  mean: 7/10 (0.7)

  var : programmer
  #f: 83/100 (0.83)
  #t: 17/100 (0.17)
  mean: 17/100 (0.17)

  var : programmer or student
  #t: 73/100 (0.73)
  #f: 27/100 (0.27)
  mean: 73/100 (0.73)

  var : programmer and student
  #f: 43/50 (0.86)
  #t: 7/50 (0.14)
  mean: 7/50 (0.14)

  var : pizza
  #f: 77/125 (0.616)
  #t: 48/125 (0.384)
  mean: 48/125 (0.384)


  * Observed that Brian ate pizza

  var : student
  #t: 49/64 (0.765625)
  #f: 15/64 (0.234375)
  mean: 49/64 (0.765625)

  var : programmer
  #f: 83/128 (0.6484375)
  #t: 45/128 (0.3515625)
  mean: 45/128 (0.3515625)

  var : programmer or student
  #t: 101/128 (0.7890625)
  #f: 27/128 (0.2109375)
  mean: 101/128 (0.7890625)

  var : programmer and student
  #f: 43/64 (0.671875)
  #t: 21/64 (0.328125)
  mean: 21/64 (0.328125)

  var : pizza
  #t: 1 (1.0)
  mean: 1 (1.0)

  So, Brian is probably a student, (the posterior is a little larger than the prior)

  * Observed that Brian did not ate pizza

  var : student
  #t: 29/44 (0.6590909090909091)
  #f: 15/44 (0.3409090909090909)
  mean: 29/44 (0.6590909090909091)

  var : programmer
  #f: 83/88 (0.9431818181818182)
  #t: 5/88 (0.056818181818181816)
  mean: 5/88 (0.056818181818181816)

  var : programmer or student
  #t: 61/88 (0.6931818181818182)
  #f: 27/88 (0.3068181818181818)
  mean: 61/88 (0.6931818181818182)

  var : programmer and student
  #f: 43/44 (0.9772727272727273)
  #t: 1/44 (0.022727272727272728)
  mean: 1/44 (0.022727272727272728)

  var : pizza
  #f: 1 (1.0)
  mean: 0 (0.0)

  Then it's a little less likely that Brian is a student, and we are
  quite sure that he's not a programmer (p =  0.0568181818181818).

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (enumerate
   
   (define student (flip 7/10))
   (define programmer (if student (flip 2/10) (flip 1/10)))
   (define pizza (if (and student programmer) (flip 9/10) (flip 3/10)))

   ; Brian ate pizza last night
   ; (observe/fail pizza)
    
   ; Brian did not ate pizza last night
   (observe/fail (not pizza))
    
   (list student
        programmer
        (or programmer student)
        (and programmer student)        
        pizza
    )

   )
)

(show-marginals (model)
                (list  "student"
                       "programmer"
                       "programmer or student"
                       "programmer and student"
                       "pizza"
                       )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


