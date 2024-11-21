#| 

  Italian murder mystery in Racket/Gamble 

  From Bellodi et.al  "Nonground Abductive Logic Programming with Probabilistic Integrity Constraints"
  https://arxiv.org/pdf/2108.03033.pdf
  Page 2
  """
  Example 1
  Several years ago, a murder in Italy captured the attention of the population: a woman was
  murdered, and the main indicted person was her husband. The collected evidence included the
  following facts: the woman was killed in the house where she lived with her husband (house1); a
  pillow stained with the blood of the victim was found in another house (house2) some hundreds
  of km away; the husband had the keys of this second house.

  ... 
  (Page 12)
  This solution (the most likely) states that the husband was the killer with a chance of 91%.
  ...
  This solution(much less probable) states that some unknown person entered the two houses 
  and committed the murder with a chance of 9%.
  """

  var : killed
  (#t #f): 4459/4918 (0.906669377795852)
  (#f #t): 459/4918 (0.09333062220414803)

  var : enter husband
  (#t #t): 4459/4918 (0.906669377795852)
  (#f #t): 189/4918 (0.038430256201708014)
  (#t #f): 189/4918 (0.038430256201708014)
  (#f #f): 81/4918 (0.016470109800732005)

  var : enter other
  (#f #f): 2401/4918 (0.4882065880439203)
  (#f #t): 1029/4918 (0.20923139487596584)
  (#t #f): 1029/4918 (0.20923139487596584)
  (#t #t): 459/4918 (0.09333062220414803)

  var : killed husband
  #t: 4459/4918 (0.906669377795852)
  #f: 459/4918 (0.09333062220414803)
  mean: 4459/4918 (0.906669377795852)

  var : killed other
  #f: 4459/4918 (0.906669377795852)
  #t: 459/4918 (0.09333062220414803)
  mean: 459/4918 (0.09333062220414803)

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
    
   ; The probability that a person (husband or some other) enters a house
   ; and did the killing
   (defmem (enter person house) 
     (if (eq? person "husband") 
         ; The husband has keys to both houses
         (flip 7/10)
         ; Someone else had to do a break in to enter the house
         (flip 3/10)))
   
   ; Probability that a person (husband or some other) killed, i.e.
   ; entered both houses.
   (defmem (killed person) (and (enter person "house1") (enter person "house2")))
   
   ; Exactly one person killed the wife, either the husband or somebody else.
   (observe/fail (xor (killed "husband") (killed "other")))
   
   (list (list (killed "husband") (killed "other"))
         (list (enter "husband" "house1") (enter "husband" "house2"))
         (list (enter "other" "house1") (enter "other" "house2"))
         (killed "husband")
         (killed "other")
         )
   )
  )

(show-marginals (model)
                (list  "killed"
                       "enter husband"
                       "enter other"
                       "killed husband"
                       "killed other"
                     )
                    #:num-samples 1000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:hpd-interval (list 0.84)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )


