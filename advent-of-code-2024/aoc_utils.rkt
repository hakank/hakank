#| 

  AoC utils in Racket.

  Advent of Code utilities

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))
(require math)

; Read a file with of space separated integers
(define (read-integer-file file)
  (map (lambda (v) (map string->number (string-split v))) (file->lines file)))

; Transpose a list of lists
(define (transpose x) (apply map list x))

; Sum a list of integers
(define (sum x) (apply + x))

(define (prod x) (apply * x))

; Converts a boolean to integer (0 or 1)
(define (b2i x) (if x 1 0))

; Wrapper for display
(define (show . a) (displayln a))

; Return min value of list a
(define (min-list a) (apply min a))

; Return max value of list a
(define (max-list a) (apply max a))

; Return the (first) differences of list lst
; This can surely be written in a neater way...
(define (differences lst)
  (for/list ([i (range 1 (length lst))])
    (- (list-ref lst i) (list-ref lst (sub1 i)))))


; Remove the element in index ix in list a
(define (remove-index ix a)
  (for/list ([i (length a)]
             #:when (not (= i ix)))
    (list-ref a i)))


#|
  (chunks-of a n #:partition? [partition? #f])
  Returns chunks of length n for a list a.
  There are two modes:
  * partition? #t (default)
    Returns a list of partitions in the list a of length n 
  * partition? #f
    Returns a list of sublist (overlapping) in the list a of length n 

  Note: For the partiton mode, the last chunk may be of length < n.

  Examples:
  > (chunks-of (range 11) 3)
  ((0 1 2) (3 4 5) (6 7 8) (9 10))
  > (chunks-of (range 11) 3 #:partition? #f)
  -> ((0 1 2) (1 2 3) (2 3 4) (3 4 5) (4 5 6) (5 6 7) (6 7 8) (7 8 9) (8 9 10)))
|#
(define (chunks-of a n #:partition? [partition? #f])  
  (define (loop x aux)
    (if (<= (length x) n)
        (append (reverse aux) (list x))
        (if partition?
            (loop (drop x n) (cons (take x n) aux))
            (loop (rest x) (cons (take x n) aux))
            )))
  (loop a '()))


(define (count-occurrences val lst)
  (count (lambda (v) (= v val)) lst)
  )

(define (count-occurrences-eq val lst)
  (count (lambda (v) (equal? v val)) lst)
  )
(define (list-slice lst [offset 0] [n (- (length lst) offset)] )
  (take (drop lst offset) n))

(define (count-occurrences-sublist sub lst)
  (let* ([sub-len (length sub)]
         [s (for/list ([i (add1 (- (length lst) sub-len))])      
              (b2i (equal? (list-slice lst i sub-len) sub))
              )])
    (if (empty? s) 0 (sum s))))

; Reference in a 2d matrix: m[i j]
(define (list-ref-2d m i j) (list-ref (list-ref m i) j))


; Return the diagonal of a (square) matrix
; There are in total 2 * num-diagonals diagonals (from left/right/up/down)  
(define (all-diagonals m)

  (let* ([n (length m)]
         [num-diagonals (- (* n 2) 1)])
    (append (for/list ([k num-diagonals])
      (for*/list ([i n]
                  [j n]
                  #:when (= (+ i j) k))
        (list-ref-2d m i j)))
    (for/list ([k num-diagonals])    
      (for*/list ([i n]
                  [j n]
                  #:when (= (+ i (- n j)) k))
        (list-ref-2d m j i)))
      )))

