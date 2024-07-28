#lang racket


;;; math/number-theory contains permutations/1 which conflicts with racket's permutation/2.
(require (rename-in math/number-theory
                    (permutations math-permutations)
                    ))

;;; Sigh! permutations is in both math and racket/list!
;;; How do I fix this is the best possible way?
;; (require (except-in racket/list math)
;;;         (racket/list))
;;; See how to fix this in euler2.rkt

;; (provide my-fib
;;          hash-append!
;;          )
(provide (all-defined-out))

;;; If excluding some provides
;;; (provide (except-out fun1 fun2)

;;
;; (run-function fun)
;;
;; Run as
;;   (run-function euler1a)
;; Is there a function of timing returning the values instead of just printing it?
;;
(define (time-function fun)
  "Run function fun with timing"
  (displayln fun)
  (time (displayln (fun)))
  (newline)
  )


;;
;; (palindromic? lst)
;;
;; Returns #t if lst is a palindromic list / string
;;
(define (palindromic? lst)
  "Returns #t if lst is a palindromic string"
  (cond
    [(string? lst) (let ([lst2 (string->list lst)]) (palindromic? lst2))]
    [else (equal? lst (reverse lst))]
    )
  )

;;;
;;; (triangle-number n)
;;; Returns the n'th triangle number.
;;;
;;; Note: This is not very efficient.
;;; See euler12.rkt for a faster version of using triangle numbers.
;;;
;; This is already defined in the math package
;; (define (triangle-number n)
;;   (for/sum ([i (range 1 (add1 n))])
;;     i)
;;   )



;;; Note: there's a much faster version in the math module: fibonacci
(define (my-fib n)
  (cond [(> n 2) (+ (my-fib (- n 2)) (my-fib (- n 1))) ]
        [else 1]))

;;;
;;; (range n)
;;; Returns the range of
;;;
(define (range1 . r)
  (cond
    [(= (length r) 1)
     (range (add1 (first r)))]
    [else
     (range (car r) (add1 (cadr r)))]
    ))
    

;;;
;;; (number->digits n)
;;; Return the digits of the number n
;;; Example:
;;; > (number->digits 12456)
;;; '(1 2 4 5 6)
;;;
(define (number->digits n)
  (map (lambda (c) (- (char->integer c) 48)) (string->list (number->string n)))
  )

;;;
;;; (digits->number digits)
;;; Returns the number consisting of the digits digits.
;;;
;;; Example:
;;; > (digits->number '(1 2 3 4 5))
;;; 12345
;;;
;;; (Note: I assume that this could be written in a neater way.)
(define (digits->number digits)
  (string->number (apply string-append (map number->string digits)))
  )

;;;
;;; (integer-length n)
;;; Return the length of integer n.
;;;
(define (integer-length n)
    (+ 1 (floor (/ (log n) (log 10))))
  )

;;;
;;; (in-range n from to)
;;; Checks if n in in the range from,,to (including)
;;;
(define (in-range n from to)
  (and (>= n from) (<= n to)))

;;; Convenience form of (floor (/ ...))
(define (div . lst)
  (floor (apply / lst)))

;;; Convenience form of (floor (/ ...))
(define (// . lst)
  (floor (apply / lst)))

;;; Day of week, Sakamoto's method
;;;  http:%en.wikipedia.org/wiki/Weekday_determination#Sakamoto.27s_Method
(define (day-of-week year month day)
  (let ([t '(0 3 2 5 0 3 5 1 4 6 2 4)]
        [yy (if (< month 3) (sub1 year) year)])
    (modulo (+ yy
               (div yy 4)
               (- (div yy 100))
               (div yy 400)
               (list-ref t (sub1 month))
               day)
            7)
    )
  )


;;
;; (hash-append! h k v)
;; Append the value v to the hash h for key k
;;
;; See insurance_salesman_problem.rkt for an example
;;
(define (hash-append! h k v)
  "Append the value v to the hash h for key k"
  (if (hash-has-key? h k)
      (hash-set! h k (append (hash-ref h k) (list v)))
      (hash-set! h k (list v))
      )
  )


;;;
;;; (chunks-of lst size)
;;; Returns a list of all size sized sub lists.
;;;
;;; Example:
;;; > (chunks-of (range 1 10) 4)
;;; '((1 2 3 4) (2 3 4 5) (3 4 5 6) (4 5 6 7) (5 6 7 8) (6 7 8 9))
;;;
(define (chunks-of lst size)
  (let* ([len1 (length lst)]
         [len (add1 (- len1 size))])
    (for/list ([i (range 0 len)]) (map (lambda (e) (list-ref lst e)) (range i (+ i size))))
    )
  )

(define (vector-chunks-of lst size)
  (let* ([len1 (length lst)]
         [len (add1 (- len1 size))])
    (for/list ([i (range 0 len)]) (map (lambda (e) (vector-ref lst e)) (range i (+ i size))))
    )
  )

;;;
;;; (vector-ref-range v from to)
;;; Returns the vector v[from..to-1] where v is a vector
;;;
(define (vector-ref-range v from to)
  (for/vector ([i (range from to)])
    (vector-ref v i))
  )


;;;
;;; (list-ref2d m i j)
;;; Returns m[i,j]
;;;
(define (list-ref2d m i j)
  (list-ref (list-ref m i) j)
  )


;;;
;;; (list-transpose m)
;;; Transposes a 2d list.
;;; 
;;; Example:
;;; > (list-tranpose ((1 4 7) (2 5 8) (3 6 9)))
;;; ((1 4 7) (2 5 8) (3 6 9))
;;;
(define (list-transpose m)
  (let ([rows (length m)]
        [cols (length (list-ref m 1))])
    (for/list ([i (range rows)])
      (for/list ([j (range cols)])
        (list-ref2d m j i)
        )
      )
    )
  )

;;;
;;; (pandigital lst n)
;;;
;;; Returns #t if lst (a list of string) is pandigital, i.e.
;;; contains n different digits.
;;; Note: We have to separate the case of a list and a string.
;;;
(define (pandigital lst [n (length lst)])
  (cond
    [(string? lst)
     (= (string-length lst) (length (remove-duplicates (string->list lst))) n) ]
    [else (= (length lst) (length (remove-duplicates lst)) n)]
    )
  )

;;;
;;; ((slice lst offset n)
;;;
;;; Returns the slice of the list lst from position offset and takes n elements.
;;; Optional parameters:
;;; - offset: defaults to 0
;;; - n: to end of list
;;;
;;; Example:
;;; > (slice '(1 2 3 4 5) 1)
;;; '(2 3 4 5)
;;; > (slice '(1 2 3 4 5) 1 2)
;;; '(2 3)
;;;
(define (slice lst [offset 0] [n (- (length lst) offset)] )
  (take (drop lst offset) n))

;;;
;;; (vector-slice v [index 0] [n 1])
;;; Returns the vector slice of size n from index index: v[index..index+n-1]
;;;
(define (vector-slice v [index 0] [n 1])
  (for/list ([i (range n)])
    (vector-ref v (+ index i))
    )
  )

;;;
;;; (string-slice s [index 0] [n 1])
;;; Returns the string slice of size n from index index: s[index..index+n-1]
;;;
(define (string-slice s [index 0] [n 1])
  (apply string (for/list ([i (range n)])
                  (string-ref s (+ index i))
                  ))
  )


;;;
;;; (string-last s n)
;;; Returns the last n characters of string s (as a string)
;;;
(define (string-last s n)
  (let ([len (string-length s)])
    (string-slice s (- (string-length s) n) n)
    )
  )


;;;
;;; (rotate lst i)
;;; Returns lst right-rotated i steps.
;;; Optional parameters:
;;; - i: 1
;;;
;;; Example:
;;; > (rotate '(1 2 3 4 5) 1)
;;; '(2 3 4 5 1)
;;;
;;; > (rotate '(1 2 3 4 5) 2)
;;; '(3 4 5 1 2)
;;;
;;; > (rotate '(1 2 3 4 5))
;;; '(2 3 4 5 1)
;;;
(define (rotate lst [i 1])
  (append (slice lst i) (slice lst 0 i))
)

;;;
;;; (list<=? lst1 lst2)
;;; Returns #t if list lst1 is lexicogrphic smaller or equal than list lst2.
;;;
(define (list<=? lst1 lst2)
  (let ([len1 (length lst1)]
        [len2 (length lst2)])
    (cond
      [(and (null? lst1) (null? lst2)) #t]
      [(< len2 len1) (and (writeln "2") #f)]
      [(and (null? lst1) (not (null? lst2))) #f]
      [(and (= (car lst1) (car lst2))) (list<=? (cdr lst1) (cdr lst2))]
      [else (<= (car lst1) (car lst2)) ]
      )
    )
  )


;;;
;;; (primes limit)
;;; Returns all primes below and including limit
;;;
;; (Why isn't this implemented in math/number-theory? (Or have I missed some variant?)
;;;

;; ;;; THIS IS TOO SLOW!
;; (define (primes-slow1 limit)
;;   (let ([p 1])
;;     (for/sum ([n (in-naturals)]
;;                #:do [(set! p (next-prime p))]
;;                #:break (> p limit))
;;       p)
;;     ))
;;
;; ;; Even slower
;; (define (primes-slow2 limit)
;;   (for/list ([n (range 0 limit)]
;;              #:break (> (nth-prime n) limit))
;;     (nth-prime n)
;;   ))

;;; This is quite fast
(define (primes n0)
  (let* ([n (add1 n0)]
         [a (make-vector n 0)])
    (for* ([i (range 2 (add1 (sqrt n)))]
           [j (range (* i i) n i)])
      (vector-set! a j 1)
      )
    (for/list ([i (range 2 n)]
               #:when (= (vector-ref a i) 0))
      i)
    ))


;;; Returns the product of the list lst
(define (list-product lst)
  (apply * lst)
  )

;;; Returns the sum of the list lst
(define (list-sum lst)
  (apply + lst)
  )

;;;
;;; (permutations-numbers-lex lst comp)
;;; Returns the permutation of digits in lexicographic order (compared with comp, < or >)
;;; Note: It returns the numbers, not the digits since it's the most common use-case.
;;;
(define (permutations-numbers-lex lst comp)
  (sort (map digits->number (permutations lst)) comp)
  )

;;;
;;; (sum-proper-divisors n)
;;; Returns the  proper divisors of n,
;;; which does not include n.
;;;
(define (sum-proper-divisors n)
  (- (list-sum (divisors n)) n)
  )

;;;
;;; (amicable n)
;;;
;;; Returns #t if n is an amicable number, i.e.
;;;  a is the sum of the proper divisors of n
;;;  b is the sum of the proper divisors of a
;;;  b == n && a != b
;;;
(define (amicable n)
  (let* ([a (sum-proper-divisors n)]
         [b (sum-proper-divisors a)])
    (and (= n b) (not (= a b)))
    )
  )



;;;
;;; (vector-swap a i j
;;; Swaps a[i] <-> a[j] in vector a.
;;;
(define (vector-swap a i j)
  (let ([t (vector-ref a i)])
    (vector-set! a i (vector-ref a j))
    (vector-set! a j t)
   )
  )

;;;
;;; (vector-next-permutation p)
;;; Generate the next (lexicographic) permutation of vector p
;;;
(define (vector-next-permutation p)

  ;;; init and i
  (let* ([len (vector-length p)]
         [len1 (sub1 len)]
         [i (sub1 len)])
    (for ([t (range (* 2 len1))]
          #:break (<= i 0)          
          #:when (and (> i 0) (>= (vector-ref p (sub1 i)) (vector-ref p i))))
      (set! i (sub1 i)))

    ;;; j
    (when (> i 0)
      (let ([j len1])
        (for ([t (range len)]
              #:break (or (< j 0) (<= i 0))              
              #:when (<= (vector-ref p j) (vector-ref p (sub1 i))))
          (set! j (sub1 j)))
        
        ;;; swap
        ;; (let ([t (vector-ref p (sub1 i))])
        ;;   (vector-set! p (sub1 i) (vector-ref p j))
        ;;   (vector-set! p j t))
        (vector-swap p (sub1 i) j)
        (let ([j len1])
          (for ([t (range (* 2 len))]
                #:break (or (< j 0) (< i 0))
                #:when (and (> j 0) (< i j)))
            (vector-swap p i j)
            (set! i (add1 i))
            (set! j (sub1 j)))
          )      
      ))

    p)
      
    )

;;;
;;; (vector->number v)
;;; Convert a vector of digits to a number
;;;
(define (vector->number v)
  (digits->number (vector->list v)))

