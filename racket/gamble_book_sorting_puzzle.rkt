#| 

  Book sorting puzzle in Racket/Gamble 

  From Rajul Saxena
  "The Library Book Sorting Puzzle"
  https://medium.com/puzzle-sphere/book-sorting-interview-puzzle-math-logic-google-amazon-microsoft-faang-quantitative-aptitude-probability-124b5917e059
  """
  Imagine you’re working at a library sorting books. You start with 32 fiction books 
  and 17 non-fiction books in a box. Each turn, you randomly pick two books from 
  the box and remove them.

  You do one more operation based on what books you picked.
  * If the books are of the same type (both fiction or both non-fiction), 
    you add a new fiction book to the box.

  * If the books are of different types (one fiction and one non-fiction), 
    you add a new non-fiction book to the box.

  * You have an unlimited supply of fiction and non-fiction books for this process.

  What will be the type of the last book remaining in the box?
  """

  * 32 fiction, 17 non-fiction
  variable : res
  (n): 1.0

  I.e. the last book will be a non fiction book

  * 32 fiction, 18 non-fiction
  variable : res
  (f): 1 (1.0)

  I.e. the last book will be a fiction book

  More generally: If the number of non-fiction books is odd then the 
  last book will be a non-fiction book. If the number of non-fiction 
  books is even, then it's a fiction book.


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define num-fiction 32)
   ; (define num-fiction 31)   
   (define num-non-fiction 17)
   ; (define num-non-fiction 18)

   ;; Checking with enumerate
   ; (define num-fiction (add1 (random-integer 6)))
   ; (define num-non-fiction (add1 (random-integer 6)))

   ; (observe/fail (= (modulo num-non-fiction 2) 0))
   ; (observe/fail (= (modulo num-non-fiction 2) 1))   

   ; The parity of the number of fiction books does not matter
   ; (observe/fail (= (modulo num-fiction 2) 0))
   ; (observe/fail (= (modulo num-fiction 2) 1))   

   
   (define init (shuffle (append (rep num-fiction "f") (rep num-non-fiction "n"))))
   ; For use with enumerate (slower than shuffle)
   ; (define init (draw-without-replacement (+ num-fiction num-non-fiction) (append (rep num-fiction "f") (rep num-non-fiction "n"))))
   ; (show "init" init)

   (define (f a)
     (if (<= (length a) 1)
         a
         (let ([pick (take a 2)]
               [new-a (drop a 2)])
           (if (= (length (remove-duplicates pick)) 1)
               (f (append new-a (list "f")))  ; same book type: add fiction
               (f (append new-a (list "n"))))) ; different book type: add non-fiction
         
         ))

   (define res (f init))

   (list res
         )
   
   ))
  

(show-marginals (model)
                (list  "res"
                     )
                    #:num-samples 10000
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


