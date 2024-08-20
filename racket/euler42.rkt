#|
  Euler #42 in Racket

  """
  The nth term of the sequence of triangle numbers is given by, 
      tn = 1/2*n*(n+1); 
  so the first ten triangle numbers are:

  1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

  By converting each letter in a word to a number corresponding to its 
  alphabetical position and adding these values we form a word value. For example, 
  the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value 
  is a triangle number then we shall call the word a triangle word.

  Using words.txt (right click and 'Save Link/Target As...'), a 16K text file 
  containing nearly two-thousand common English words, how many 
  are triangle words?
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

(require (only-in math/number-theory
                  triangle-number
                  ))

(require (only-in "utils_hakank.rkt"
                  time-function
                  ))

(define (conv-word word)
  (for/sum ([c word])
    (- (char->integer c) 64)
    )
  )

;;; cpu time: 14 real time: 14 gc time: 8
(define (euler42a)
  (let ([tri (for/hash ([n (range 40)]) (values (triangle-number n) 1) )])
    (for/sum ([word (string-split (string-replace (file->string "euler42_words.txt") "\"" "") ",")]
               #:when (hash-has-key? tri (conv-word word)))
    1)
    )
  )

(define (run)
  (time-function euler42a)
  )

(run)
