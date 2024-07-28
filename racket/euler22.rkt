#|
  Euler #22 in Racket

  Problem 22
  """
  Using names.txt (right click and 'Save Link/Target As...'), a 46K 
  text file containing over five-thousand first names, begin by sorting 
  it into alphabetical order. Then working out the alphabetical value 
  for each name, multiply this value by its alphabetical position in the 
  list to obtain a name score.

  For example, when the list is sorted into alphabetical order, COLIN, 
  which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in 
  the list. So, COLIN would obtain a score of 938*53 = 49714.

  What is the total of all the name scores in the file?")
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

;;; (require math/number-theory)
;;; (require racket/trace)

(require "utils_hakank.rkt")

(define (word-value word)
  (for/sum ([c (string->list word)])
    (- (char->integer c) 64)
    )
  )

(define (euler22a)
  (let ([names (sort (string-split (string-replace (file->string "euler22_names.txt") "\"" "") ",") string<?)])
    (for/sum ([i (in-naturals 1)]
               [name names])
      (* i (word-value name))
      )
    )
  )

(define (run)
  (time-function euler22a)
  )

(run)



