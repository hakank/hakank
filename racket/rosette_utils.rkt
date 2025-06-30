#| 

  Rosette utils in Racket.


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang rosette

(provide (all-defined-out))

; (require rosette/solver/smt/z3)

; Show all parameters
(define (show . a)
  (displayln a))

; Show the content of x and then return x
; For debugging an expression
(define (debug x)
  (show "debug:" x)
  x)
  


(define (show-constraints constraints)
  (for ([c constraints])
    (show "c" c)
    ))

; Show all solutions (the hash representation)
; TODO:
;  - Add a counter
;  - Add a limit on the number of solutions
;
(define (show-all-solutions vars [count 0] #:debug? [debug? #f] #:max-sols [max-sols 0] )
  (let* ([sol (solve vars)])
    ; Continue if it's a sat model
    (when (sat? sol)
        (let ([h (model sol)])
          (displayln (list (string-append "sol " (number->string (add1 count))) sol))
          (displayln (evaluate vars sol))
          (when debug?
            (displayln (list "vc" (vc)))
            )
          ; (for ([key (in-hash-keys h)]) (displayln (list "key" key "val" (hash-ref h key))))
          ; Ensure that there are at least one assignment that's not in this solution
          (assert (apply || (for/list ([key (in-hash-keys h)])
          ; (assert (apply || (for/list ([key vars])                              
                              (! (eq? key (hash-ref h key))))))
          (newline)
          (when (or (= max-sols 0) (< count (sub1 max-sols)))
            (show-all-solutions vars (add1 count) #:debug? debug? #:max-sols max-sols))
          )
    )))

; Return all solutions (the hash representation)
; TODO: Limit the number of solutions
(define (get-all-solutions vars #:debug? [debug? #f])
  ; (define solver (z3 #:logic 'qf_nia)) ; This does not work (as I understand it)
  ; (show "solver-check" (solver-check solver))
  (define (loop aux)
    (let* ([sol (solve vars)])
    (if (sat? sol)
        (let ([h (model sol)])
          (define assignments(for/list ([key (in-hash-keys h)])
                               (list key (hash-ref h key))))
          ; (displayln (list "assignments" assignments))
          (when debug? (displayln (list "h" h)))
          (assert (apply || (for/list ([key (in-hash-keys h)])
                              (! (eq? key (hash-ref h key))))))
          (loop (append aux (list (evaluate vars sol))))
          ; (loop (append aux (list h)))          
          )
        aux
        )
    ))
    (loop '())
    )


;
; Return the solution which maximizes the value opt
; Note: The optimization value _must_ be called 'opt! TODO: Fix this
; - get-all-opt?: Get all optimal solutions
(define (get-max-solution vars opt #:debug? [debug? #f] #:get-all-opt? [get-all-opt? #f])
  (show "get-max-solution" vars opt)
  (define (loop aux)
    (let* ([sol (solve vars)])
      ; (show "sol" sol)
      (if (sat? sol)
          (let ([h (model sol)]
                [ev (evaluate vars sol)])
            (define assignments(for/list ([key (in-hash-keys h)])
                                 (list key (hash-ref h key))))
            ; (displayln (list "assignments" assignments))
            ; (when debug? (displayln (list "h" h)))
            (when debug? (displayln ev))
            (define opt-val (hash-ref h opt))
            
            (if get-all-opt?
                (begin
                  (assert (>= opt opt-val))              
                  (assert (apply || (for/list ([key (in-hash-keys h)])
                                      (! (eq? key (hash-ref h key))))))
                  )
                (assert (> opt opt-val))
                )
            
            (if get-all-opt?
                (if (and (> (length aux) 0) (> opt-val (first aux)))
                    (loop (list opt-val ev))
                    (loop (append aux (list opt-val ev)))                  
                    )
                (loop (list ev))
                )
            )
          (if get-all-opt?
              (rest aux)
              aux ; Return solution
              )
          )
      ))
  (loop '())
  )

; TODO: Combine get-max-solution and get-min-solution into get-optimal-solution
(define (get-min-solution vars opt #:debug? [debug? #f] #:get-all-opt? [get-all-opt? #f])
  (show "get-min-solution" vars opt)
  (define (loop aux)
    (let* ([sol (solve vars)])
      (if (sat? sol)
          (let ([h (model sol)]
                [ev (evaluate vars sol)])
            (define assignments(for/list ([key (in-hash-keys h)])
                                 (list key (hash-ref h key))))
            ; (displayln (list "assignments" assignments))
            ; (when debug? (displayln (list "h" h)))
            (when debug? (displayln ev))
            (define opt-val (hash-ref h opt))
            
            (if get-all-opt?
                (begin
                  (assert (<= opt opt-val))              
                  (assert (apply || (for/list ([key (in-hash-keys h)])
                                      (! (eq? key (hash-ref h key))))))
                  )
                (assert (< opt opt-val))
                )
            
            (if get-all-opt?
                (if (and (> (length aux) 0) (< opt-val (first aux)))
                    (loop (list opt-val ev))
                    (loop (append aux (list opt-val ev)))                  
                    )
                (loop (list ev))
                )
            )
          (if get-all-opt?
              (rest aux)
              aux ; Return solution
              )
          )
      ))
  (loop '())
  )


(define (transpose xs) (apply map list xs))

; Reference in a matrix
(define (list-ref2d x i j)
  (list-ref (list-ref x i) j))

; Wrapper for list-ref and list-ref2d
(define (ix a i [j #f])
  (if j
      (list-ref2d a i j)
      (list-ref a i)))



; Create a 2d matrix given clues
; - clues: 2d matrix 
; - #:lower: lower domain
; - #:upper: upper domain
; - #:unknown: the value that indicates unknown value -> decision variable
;
; Note: This will define a matrix of internal values x$00 etc.
; This is handled properly by using
;  (define sol (solve #t)
;  (show "x" (evaluate x sol)
;  (show "x" (evaluate y sol)
;  ...
(define (make-grid-vars clues
                        #:lower [lower 1]
                        #:upper [upper (length clues)]
                        #:unknown [unknown 0])
  (for/list ([i (length clues)])
    (for/list ([j (length (first clues))])
        (let ([val (list-ref2d clues i j)])
          (cond [(eq? val unknown)
                 (define-symbolic* x integer?)
                 (assert (<= lower x upper))
                 x]
                [else val])))))

; Create a list of size len with domain (<= lower var upper)
(define (make-var-array-integer len lower upper)
  (for/list ([i len])
    (define-symbolic* x integer?)
    (assert (<= lower x upper))
    x
    ))

; Create a matrix of size rows x cols with domain (<= lower var upper)
(define (make-var-matrix-integer rows cols lower upper)
  (for/list ([i rows])
    (for/list ([j cols])
      (define-symbolic* x integer?)
      (assert (<= lower x upper))
      x
    )))


; Create a list of size len with domain (<= lower var upper)
; Reals
(define (make-var-array-real len lower upper)
  (for/list ([i len])
    (define-symbolic* x real?)
    (assert (<= lower x upper))
    x
    ))


(define (show-matrix x #:width [width 3] #:align [align 'right] #:replace? [replace? #f])
  (let ([rows (length x)]
        [cols (length (first x))])
    (for ([i rows])
      (for ([j cols])
        (let* ([val (list-ref2d x i j)]
               [show-val (if (not (list? replace?))
                             val
                             (if (eq? val (first replace?)) (second replace?) val))])
        (display (~a show-val
                     #:width width
                     #:align align
                     #:right-pad-string " "
                     #:left-pad-string " "))
        ))
      (newline)
      )))

; Return a list with the constraint c appended to constraints
; For use with solver-check etc
(define (add constraints c)
  (if (list? c)
      (append constraints c)
      (append constraints (list c))))
  
;
; Global constraints
;

(define (increasing a)        (apply <= a))
(define (increasing-strict a) (apply < a))
(define (decreasing a)        (apply >= a))
(define (decreasing-strict a) (apply > a))
(define (sum x)               (apply + (for/list ([v x]) v)))
(define (product x)           (apply * (for/list ([v x]) v)))

;
; (scalar-product x y)
; returns sum (x * y)
;
(define (scalar-product x y)
  (sum (map (lambda (a b) (* a b)) x y)))


; Wrapper for distinct? 
(define (all-different x)    (apply distinct? x))

; Ensure that all elements != c (default 0) are different
(define (all-different-except-c x [c 0])
  (let ([n (length x)])
    (for*/list ([i n]
                [j (range (add1 i) n)])
      (let ([xi (list-ref x i)]
            [xj (list-ref x j)])
        (assert (when (and (not (= xi c))
                           (not (= xj c)))
                  (not (= xi xj )
                       )))))))
  
