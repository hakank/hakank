#| 

  Utils for Gamble model in Racket.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

(provide (all-defined-out))

(require racket)

;; This is for rejection-query and enumeration-query
(require (for-syntax racket/base
                     syntax/parse))

(require (only-in math/statistics
                  quantile))


; Start: from Gamble's examples/forestdb/church-compat.rkt
; See gamble_schelling_coordination_game.rkt for an example.
(define-syntax rejection-query
  (syntax-parser
    [(_ def/expr ... result-expr #:when condition)
     #'((rejection-sampler def/expr ... (observe/fail condition) result-expr))]))

(define-syntax enumeration-query
  (syntax-parser
    [(_ def/expr ... result-expr #:when condition)
     #'(discrete-dist->lists
        (enumerate #:limit #f def/expr ... (observe/fail condition) result-expr))]))

(define (discrete-dist->lists dd)
  (list (vector->list (discrete-dist-values dd))
        (for/list ([v (discrete-dist-values dd)])
          (dist-pdf dd v))))

; This was called multinomial earlier, but Gamble has multinomial-dist (but not multinomial for
; some reason). Renaming to multinomial2 emphasises that it's special
; Note that this returns a single value, in contrast to (multinomial-dist n vs) which
; returns n values
(define (multinomial2 xs ws)
  (discrete* xs ws))

(define (all ls) (andmap values ls))
(define (any? ls) (ormap values ls))
(define (fold f init lst) (foldl f init lst))
(define (pair a b) (cons a b))

;;; End: from Gamble's examples/forestdb/church-compat.rkt

;;;
;;; Port of Church's uniform-draw
;;;
;; (define (uniform-draw dist)
;;   (let* ([len (length dist)]
;;          [d (for/list ([item dist])
;;               (cons item (/ 1 len)))]
;;          )
;;     (sample (make-discrete-dist d))
;;     )
;;   )

(define (uniform-draw dist)
  (discrete* dist)
  )

;;;
;;; Port of Church's gaussian
;;;
(define (gaussian mean stddev)
  (normal mean stddev))

;;;
;;; Port of Church's random-integer
;;; Returns a random integer 0..n-1
(define (random-integer n)
  (discrete-uniform n))

;;; Dist version of random-integer
(define (random-integer-dist n)
  (categorical-dist (make-vector n (/ n))))


;;
;; (categorical-vw-dist values probs)
;; Return a sample of categorical-vw-dist
;;
;; Note: values and probs should be vectors
;;
(define (categorical-vw values probs)
  (sample (categorical-vw-dist values probs))
  )

;;
;; (categorical-vw-dist values probs)
;; Return a distribution of categorical values drawn by
;; probs.
;;
;; Note: values and probs should be vectors
;;
(define (categorical-vw-dist values probs)
  (make-discrete-dist* values probs)
  )

;;
;; As categorical-vw-dist but probs is first
;; (so it's easier to port from WebPPL models)
;;
;; Note: values and probs should be vectors
;;
(define (categorical-vw-dist2 probs values)
  (make-discrete-dist* values probs)
  )

;; Sample version of categorical-vw-dist2
(define (categorical-vw2 probs values)
  (sample (categorical-vw-dist values probs))
  )



;; ; Port of Church's all
;; (define (all? lst)
;;   (andmap (lambda (v) (eq? v #t)) lst))

;; ; Port of Church's any
;; (define (any? lst)
;;   (ormap (lambda (v) (eq? v #t)) lst))

; Port of Church's some
(define (some lst)
  (any? lst)
  )




; hakank: flip as a distribution
(define (flip-dist p)
  (make-discrete-dist* (vector #t #f) (vector p (- 1 p)))
  )

;;: (sum lst)
;;; Returns the sum of list lst
(define (sum lst)
  (apply + lst))

;;; (avg lst)
;;; Return the average (mean) of lst
(define (avg lst)
  (/ (sum lst) (length lst))
  )

;;;
;;; (show-model model #:num-samples #:cred-mass #:no-dist? #:no-stats #:no-cred)
;;;
;;; A general function to show a model's distribution
;;;
;;; Parameters:
;;; - model: the model
;;; - num-samples: number of samples for the samplers, default 1000
;;; - cred-mass: the credibility mass for credible-interval, default 0.84
;;; - no-dist?: Don't show the distribution (only for samplers), default #f
;;; - no-stat?: Don't show the statistics (only for samplers), default #f
;;; - no-cred?: Don't show the credible interval (only for samplers), default #f
;;;
;;; TODO?:
;;; * For large distributions, just show the first and last n entried?
;;; 
(define (show-model model
                    #:num-samples [num-samples 1000]
                    #:cred-mass   [cred-mass 0.84]
                    #:no-dist?    [no-dist? #f]
                    #:no-stats?   [no-stats? #f]
                    #:no-cred?    [no-cred? #f]                    
                    )
  (cond
    ; discrete distribution (always show this distribution)
    [(discrete-dist? model) (show-discrete-dist model) ]
    
    ;; All the rest - rejection/importance/mh-samplers - are considered weighted samplers
    ;; (Only importance-sample is NOT considered a weighted-sampler!)
    [(weighted-sampler? model)
     ; show the distribution?
     (when (not no-dist?) (show-discrete-dist (sampler->discrete-dist model num-samples)))

     ;; show statistics and/or credible interval ?
     (when (or (not no-cred?) (not no-stats?))
       (let ([samples (vector->list (generate-samples model num-samples))])
         (when (not no-stats?) (show-stats samples))
         (when (not no-cred?) (show-credible-interval samples cred-mass))
         ))
     ]
    [else (displayln "Not a distribution model!")]
    )
  (newline)
  )

;;;
;;; (show-discrete-dist dist)
;;; Pretty print a discrete-distribution, show in decreasing order of the probabilities.
;;; If the values (vals) contains of plain numbers, then the (weighted) mean is also printed.
;;;
(define (show-discrete-dist dist)
  (let* ([vals (vector->list (discrete-dist-values dist))]
         [weights (vector->list (discrete-dist-weights dist))]
         [sorted (sort (map (lambda (a b) (list a b)) vals weights) > #:key cadr)]
         )
    (if (exact? (first weights))
        (map (lambda (s) (displayln (format "(~a : ~a (~a))" (first s) (second s) (exact->inexact (second s))))) sorted)
        (map (lambda (s) (displayln (format "(~a : ~a)" (first s) (second s)))) sorted)
        )
    ;;; if values just contains single numbers: calculate the mean
    (when (number? (first vals))
      (let ([total (for/sum ([e vals]) e)]
            [mean (sum (map (lambda (val weight) (* val weight)) vals weights))]
                       
            )
        (displayln (list "mean:" (* 1.0 mean)))
        )
      )
    )
  
  #t
  )

;;
;; (show-marginals model vars
;;                        #:num-samples
;;                        #:truncate-output
;;                        #:credible-interval
;;                        #:show-stats?
;;                        #:skip-marginals?
;; )
;;
;; Show the marginal probabilities for a model with a list of the name of the return values.
;; Parameters
;; - model: the model
;; - vars: the name of the variables in the return list
;; - num-samples n: number of samples
;; - truncate-output n: only show the first n and the last n rows in the distribution
;; - show-stats?: default #f
;; - credible-interval: the credible mass, default #f (i.e. don't show)
;; - skip-marginals?: If #t then no marginals is shown
;;
;; Note: Both show-stats and credible-interval works on a (different) sample set
;;       than the rest of the program.
;;       This might mean that the two means might differ.
;;
;; TODO: Figure out how to calculate credible interval using vals and weights instead
;;       of running a new sample collection.
;;
(define (show-marginals model
                        vars
                        #:num-samples [num-samples 1000]
                        #:truncate-output [truncate-output 0]
                        #:credible-interval [credible-interval #f]
                        #:credible-interval2 [credible-interval2 #f]                        
                        #:show-stats? [show-stats? #f]
                        #:skip-marginals? [skip-marginals? #f]
                        #:show-histogram? [show-histogram? #f]
                        #:show-percentiles? [show-percentiles? #f]
                        )
  ; Convert to a discrete distribution (if needed)
  (let* ([res (if (discrete-dist? model) model (sampler->discrete-dist model num-samples))]
         [vals (vector->list  (discrete-dist-values res))]
         [weights (vector->list  (discrete-dist-weights res))]
         [first-val (first (first vals))]
         [num-vars (length (first vals))]
         [h (for/list ([i (range num-vars)]) (make-hash))]
         [samples (if (or credible-interval credible-interval2 show-stats? show-histogram? show-percentiles?)
                      (if (discrete-dist? model)
                          (repeat (lambda() (sample model)) num-samples)
                          (vector->list (generate-samples model num-samples))
                          )
                      '()
                      )
                  ]
         )
    
    ;; Collect the probabilities into the h hash for the variables and values
    (map (lambda (v w)
           (for ([i (range num-vars)])
             (let ([key (list-ref v i)])
               ; update the i'th hash
               (hash-update! (list-ref h i) key (lambda (val) (+ val w)) 0))))
         vals weights
         )
    ;; Loop through the variables and print all values and probabilities
    ;; ordered by decreasing probability.
    ;; Also, calculate the mean value
    (for ([i (range num-vars)])
      (show "var " (list-ref vars i))
      
      (let* ([mean-val '()]
             [this-hash  (list-ref h i)]
             [this-hash-length  (length (hash-keys this-hash))]
             [this-hash-sorted (hash-sort this-hash >)]
             [this-sample (map (lambda (v) (list-ref v i )) samples)]
             )

        ; Show all "key : probability"
        (for ([j (length this-hash-sorted)])
          (let* ([kv (list-ref this-hash-sorted j)]
                 [key (car kv)]
                 [prob (cdr kv)]
                 [key-is-symbol-or-string?
                  ; (or (symbol? key) (string? key) (list? key))
                  (not (or (number? key) (boolean? key)))
                  ]
                 [key-val (if (boolean? key) (if (eq? key #t) 1 0) key)]                 
                 )
            ;; Handle truncate-output
            (when (not skip-marginals?)
                (when (or (= truncate-output 0)
                          (< j truncate-output)
                          (> j (sub1 (- this-hash-length truncate-output))))
                  ;; Handle exact probabilities (-> convert to inexact)              
                  (if (exact? prob)
                      (displayln (format "~a: ~a (~a)" key prob (exact->inexact prob)))
                      (displayln (format "~a: ~a" key prob))))
                ;; Show the mid "..." in the output
                (when (and (> truncate-output 0)
                           (= truncate-output j))
                  (displayln "..."))
            )

            ;; Mean values are only for numeric values and boolean, not for symbols/strings
            (when (not key-is-symbol-or-string?)
              ;; Messy!
              (set! mean-val (+ (if (eq? mean-val '()) 0 mean-val) (* key-val prob)))))

          )
        ; Is the key a number or boolean?
        ; We use the flag for mean-val for checking this
        (when (not (eq? mean-val '()))
          ; Mean
          (if (exact? mean-val) 
              (displayln (format "mean: ~a (~a)" mean-val (exact->inexact mean-val)))
              (displayln (format "mean: ~a" mean-val))
              )
          
          ; Show stats
          (when show-stats?
            (show-stats (for/list ([v samples])
                          (list-ref v i))))
          
          ; Credible interval
          (when credible-interval
            (show-credible-interval 
             (for/list ([v samples])
               (list-ref v i))          
             credible-interval)
            )

          
          )

          ;; ; Credible interval
          ;; (when credible-interval
          ;;   (show-credible-interval 
          ;;    (for/list ([v samples])
          ;;      (list-ref v i))          
          ;;    credible-interval)
          ;;   )

          ; Credible interval2 (alternative, but might give different results)
          (when credible-interval2
            (show-credible-interval2 
             (for/list ([v samples])
               (list-ref v i))          
             credible-interval2)
            )

        
          (when show-percentiles?
            (displayln "Percentiles:")
            (if (list? show-percentiles?)
                (show-percentiles this-sample show-percentiles?)
                (show-percentiles this-sample)
                ;; TODO
                ;; (show-percentiles vals weights show-percentiles?)
                ;; (show-percentiles vals weights)                  
                )
            )
        
          (when show-histogram?
            (displayln "Histogram:")
            (show-histogram this-sample #:num-bins show-histogram?)
            )
        
        (newline)
        )
      )        
    )
  )



#|
  (get-probs model #:num-samples [num-samples 1000] #:ix [ix #f] #:float? [float? #f]
  Returns a list of probabilities from the model model.
  - #:ix: Only return the ix'th elements
  - #:float?: Convert probabilities to floats

  (Extracted from show-marginals)
|#
(define (get-probs model #:num-samples [num-samples 1000] #:ix [ix #f] #:float? [float? #f])
  (let* ([res (if (discrete-dist? model) model (sampler->discrete-dist model num-samples))]
         [vals (vector->list  (discrete-dist-values res))]
         [weights (vector->list  (discrete-dist-weights res))]
         [num-vars (length (first vals))]
         [h (for/list ([i (range num-vars)]) (make-hash))])
    ; Collect into hash h
    (map (lambda (v w)
           (for ([i (range num-vars)])
             (let ([key (list-ref v i)])
               ; update the i'th hash
               (hash-update! (list-ref h i) key (lambda (val) (+ val w)) 0))))
         vals weights)

    ;; Loop through the variables and get all values and probabilities
    ;; ordered by decreasing probability.
    (define all '()) ; The output list
    
    (for ([i (range num-vars)])      
      (let* ([this-hash  (list-ref h i)]
             [this-hash-sorted (hash-sort this-hash >)]
             [these-probs '()])
        
        ; Fetch all "key : probability"
        (for ([j (length this-hash-sorted)])
          (let* ([kv (list-ref this-hash-sorted j)]
                 [key (car kv)]
                 [prob (cdr kv)]
                 )
            (set! these-probs (append these-probs
                                      (list (list key
                                                  (if (and (exact? prob) float?)
                                                      (exact->inexact prob)
                                                      prob)
                                                  ))))
            )
          )
        (set! all (append all (list these-probs)))
        )
      )
    (if ix 
        (list (list-ref all ix))
        all
        )
    )
  )

#|
  (probs-mean probs)

  Return the mean of a list of probabilitiy pairs (value probability),
   e.g. from (get-probs model...).

  Note: Only for numbers of booleans. Otherwise return #f

|#
(define (probs-mean probs)
  (let ([first-val (first (first probs))])
    (if (or (number? first-val) (boolean? first-val))
        (let ([sum 0])
          (for ([kp probs])
            (let* ([k1 (first kp)]
                   [k (if (boolean? k1) (boolean->integer k1) k1)]
                   [p (second kp)])
              (set! sum (+ sum (* k p)))
              )
            )
          sum
          )
        #f ; not possible to get mean value of symbols, strings etc
        )
    )
  )


;;
;; Returns the sort function for a certain type of list.
;; (assuming all values in the list is of the same type)
;; Example
;;  (sort (list 555 122 742 3387 42 15) (sort-less-than 1))
;;
(define (sort-less-than v)
  (cond
    [(string? v) string<?]
    ; There must be a simpler way of doing this...
    [(boolean? v) (lambda (x y) (if (and (eq? x #f) (eq? y #t)) #t #f))]
    [(number? v) <]
    [(symbol? v) symbol<?]
    [(list? v) string<?] ; Note: Must convert list to string in the caller
    [else <])
)

(define (sort-greater-than v)
  (cond
    [(string? v) string<?]
    ; There must be a simpler way of doing this...
    [(boolean? v) (lambda (x y) (if (and (eq? x #f) (eq? y #t)) #f #t))]
    [(number? v) >]
    [(symbol? v) (not symbol<?)]
    [(list? v) string>?] ; Note: Must convert list to string in the caller
    [else >])
)

;;
;; The built-in sort depends a lot on the type of the elements,
;; Here's a more general version.
;; Note especially that if lst contains lists, then they are first converted to strings
;; and use string<? (or string>?) for the sort comparison.
;;
(define (my-sort lst [order <] #:key [key #f])
  (let* ([first-value (first lst)]
         [comp (if (eq? order <)
                   (sort-less-than first-value)
                   (sort-greater-than first-value))]
         [lst2 (if (list? first-value) (map (lambda (v) (~a v)) lst) lst)]
         [sorted (if (not key) (sort lst2 comp #:key key) (sort lst2 comp))]
         )
    sorted
    ))


#|
  Calculate the number of bins (for histogram)

  https://en.wikipedia.org/wiki/Histogram

|#
;; Square root choicexb
;; num_bins(L) = ceiling(sqrt(L.len)).

;; Rice Rule seems to be a little better...
(define (num-bins values) 
  (* 2 (inexact->exact (ceiling (expt (length values) (/ 3.0)))))
)


#|
  This is a port of my WebPPL function histogram 
  (in node_modules/hakank_utils/hakank_utils.js).

  Note: exact values are handed as symbolic values as they are distinct.
|#
(define (histogram values num_bins)
  (if (and (number? (first values))
           (not (exact? (first values)))
           (not (list? (first values)))
           )
      ;; numeric values
      (let* ([minv (apply min values)]
             [maxv (apply max values)]
             [num-distinct-values (length (remove-duplicates values))]
             [num_bins (min num-distinct-values
                        (if (exact? (first values))
                           (min num_bins (length (remove-duplicates values)))
                           num_bins))
                       ]
             [width (/ (- maxv minv) num_bins)]             
             [bins (make-vector (add1 num_bins) 0)]
             ;; ;; Special handling of integers vs floats
             [isInt (exact? (first values))]
             [intervals (if isInt
                            (for/list ([v num_bins])
                             (round  (+ (* v width) minv)))
                            (for/list ([v num_bins])
                              ; (round (/ (* (+ (* v width) minv) 10000.0) 10000.0)) )
                              (~r (+ (* v width) minv) #:precision 3))
                            )
                        ]
             )

        ; Fix for a single unique numeric value
        (if (= num-distinct-values 1)
            (set! bins (make-vector 1 (length values)))
              (for ([v values])
                (let ([t (inexact->exact (ceiling (/ (- v minv) width)))])
                  (vector-set! bins t (add1 (vector-ref bins t)))
                  ))
            )
        (list bins intervals)
        )
      ;; else: symbolic values
      (begin
        (let* ([bins (collect values)]
               [keys (hash-keys bins)]
               [values (for/list ([key keys]) (hash-ref bins key))]
               [k1 (first keys)]
               ;; [lt
               ;;  (cond
               ;;    [(string? k1) string<?]
               ;;    ; There must be a simpler way of doing this...
               ;;    [(boolean? k1) (lambda (x y) (if (and (eq? x #f) (eq? y #t)) #t #f))]
               ;;    [(number? k1) <]
               ;;    [(symbol? k1) symbol<?]
               ;;    [(list? k1) string<?]                  
               ;;    [else <])
               ;;  ]
               )
          ; (list (list->vector values) (sort keys lt))
          ;; (if (list? k1)
          ;;     (list (list->vector values) (sort (map (lambda (v) (~a v)) keys) (sort-less-than k1))) ; must convert to string (I think)
          ;;     (list (list->vector values) (sort keys (sort-less-than (first keys))))
          ;;     )
          ; A more generic version of sort, which handles different types of elements
          (list (list->vector values) (my-sort keys))
           
          )
        
        )
      )        
  )


(define (show-histogram samples #:num-bins [num-hist-bins #f])
  (let* ([nbins (if (number? num-hist-bins)
                    num-hist-bins
                    (num-bins samples))]
         [hist (histogram samples nbins)]
         [counts (first hist)]
         [keys (second hist)]
         ; [max-key (last keys)]
         ; Get the longest key (a little too convoluted...)
         [max-key
          (second (last (sort (map (lambda (v) (list (string-length (~a v)) v)) (remove-duplicates keys) ) #:key first <)))
                         ]
         [max-count (vector-argmax identity counts)]
         [max-key-len (string-length (~a max-key))]
         [max-count-len (string-length (~a max-count))]
         )
    ;; (show2 "nbins" nbins)
    ;; (show2 "keys" keys)    
    ;; (show2 "counts" counts)    
    ; (show2 "max-key" max-key "max-count" max-count)    
    ; (show2 "max-key-len" max-key-len "max-count-len" max-count-len)
    (for ([i (length keys)])
      (let ([key (list-ref keys i)]
            [count (vector-ref counts i)] )
        (displayln (format "~a: ~a"
                           (if (string? key)
                               (~a key    #:width max-key-len #:align 'left)
                               (~a key    #:width max-key-len #:align 'right)                               
                               )
                           (~a count  #:width max-count-len #:align 'left)
                         ))
        )
      
      )
    )
  )

#|
   Percentiles of an array.

   Example usage:
        var model = function() {
            // ...
            return {x:x}
        }
        var d = Infer(model)
        var m = marginalize(d, "x");
        var s = m.supp;
        var ps = [0,2.5,25,50,75,97.5,100];
        var pcts = percentile(s,ps);

   From https://en.wikipedia.org/wiki/Percentile
   Using Nearest rank method
|#
(define *default-percentile-ps* (list 0.01 0.025 0.1 0.05 0.25 0.5 0.75 0.84 0.9 0.95 0.975 0.99 0.999))
(define (percentiles values [ps *default-percentile-ps*])
  ; (show2 "percentiles values" values "ps" ps)
  (let* ([len (length values)]
         ; [sorted (sort values (sort-less-than (first values)))] ; TODO: different types on sort
         [sorted (my-sort values)] 
         )
    (for/list ([p ps])
      (let ([v (inexact->exact (ceiling (* len p) ))])
        (list p (list-ref sorted (sub1 (max 1 v))))
        )
      )
    )
  )

;; TODO! Use quantile from math/statistics instead
;; (define (percentiles2 xs ws [ps *default-percentile-ps*])
;;   ; (show2 "percentiles" xs ws ps)
;;   (for/list ([p ps])
;;     (list p (quantile p < (flatten xs) ws))
;;   ))


(define (show-percentiles values [ps *default-percentile-ps*])
  (for ([p (percentiles values ps)])
    (displayln p)
    )
  )

;; TODO
;; (define (show-percentiles values weights [ps *default-percentile-ps*])
;;   (for ([p (percentiles2 values weights ps)])
;;     (displayln p)
;;     )
;;   )

#|
  (dist-quantiles dist qs [n 1000])
  Quantiles of a distribution based on samples
  Returns the quantiles given by qs for the distribution dist.
  The number of samples is n (default 1000).

  Note: The disttribution should be defined in a lambda function, e.g.
   (define qs '(0.001  0.01 0.02 0.05 0.10 0.25 0.50 0.75 0.90 0.95 0.98 0.99 0.999))
   (dist-quantiles (lambda () (normal 100 15)) qs num-samples)

  TODO: Use math/statistics/quantile instead?
|#
(define (dist-quantiles dist qs [n 1000])
  (define (model)
    (importance-sampler
     (dist)
     ))
  (define s (make-samples (model) n))
  ; (percentiles s qs)
  (for/list ([q qs])
    (list q (quantile q < s))
    )
  
  )



;;;
;;; (credible-interval samples cred-mass)
;;; Returns the credible interval of samples `samples  with
;;; the credibility mass of `cred-mass`
;;;
;;; (This is a port of my WebPPL function credible-interval . )
;;; 
;;; TODO:
;;;  - if there are too few example and/or a very large/small cred-mass
;;;    then ci-widths might be empty.
;;; 
(define (credible-interval samples cred-mass)
  (when (member (first samples) '(#t #f))
      (set! samples (samples-to-01 samples)))  
  (let* ([sorted-pts (sort samples <)]
         [len (length sorted-pts)]
         [ci-idx-inc (inexact->exact(ceiling (* cred-mass len)))]
         [n-CIs (- len ci-idx-inc)]
         ; TODO: Fix this (see above)
         [ci-widths (for/list ([i (range n-CIs)])
                      (- (list-ref sorted-pts (+ i ci-idx-inc)) (list-ref sorted-pts i))
                      )]
         ; Temporary fix by checking if empty ci-widths
         [ix (if (empty? ci-widths) -1 (index-of ci-widths (apply min ci-widths)))]
         )
    (if (= ix -1)
        (and (displayln "Unexpected error in credible-interval! Try a larger sample or smaller cred-mass.") '( () ()))
        (list (list-ref sorted-pts ix) (list-ref sorted-pts (+ ix ci-idx-inc)))
        )
    )
  )

;;;
;;; (show-credible-interval samples cred-mass)
;;; Outputs the credible interval for samples and credible mass
;;;
(define (show-credible-interval samples cred-mass)
  (let ([ci (credible-interval samples cred-mass)])
    (displayln (format "Credible interval (~a): ~a..~a" cred-mass (first ci) (second ci)))
    )
  )

;;;
;;; Since there are issues with credible-interval for small samples and large/small
;;; cred masses, I'm trying to use percentiles instead which don't have this
;;; issue.
;;; But it don't show the same interval as credible-interval...
;;; 
(define (credible-interval2 samples cred-mass)
  (let ([ps (let* ([t (/ cred-mass 2)]
                   [t0 (- 0.5 t)]
                   [t1 (+ 0.5 t)])
              (list t0 t1 ))])
    ps
    )
  )

(define (show-credible-interval2 samples cred-mass)
  (let ([ps (credible-interval2 samples cred-mass)])
    ; (show2 "ps" ps)
    (let {[perc (map second (percentiles samples ps))]}
      (displayln (format "Credible-interval2 (~a): ~a..~a (ps: ~a)" cred-mass (first perc) (second perc) ps ))
      )
    )
  )


; Convert #t/#f list to 1/0
(define (samples-to-01 s)
  (map (lambda (v) (if v 1 0)) s))

;;
;; Returns num-samples from the model.
;; * num-samples: the number of samples to return
;; * ix: If the samples is a list, take the ix'th elements of each sample
;; * num-internal-samples: for samplers, the number of samples to make a discrete-distribution from
;;
(define (make-samples model num-samples #:num-internal-samples [num-internal-samples 30] #:ix [ix #f] )
  ; Handling a "pure" discrete model is a little different from handling a sampler model...
  (let ([samples (if (discrete-dist? model)
                     (repeat (lambda () (sample model)) num-samples)
                     (repeat (lambda () (sample (sampler->discrete-dist model num-internal-samples))) num-samples)
                     )])
    (if (and ix (list? (first samples)))
        (map (lambda (v) (list-ref v ix)) samples)
        samples
        )
    )
  ) 

;;
;; (show-stats samples)
;; Prints some statistics of samples
;; - min
;; - mean
;; - max
;; - variance
;; - stddev
;;
(define (show-stats samples)
  (when (member (first samples) '(#t #f))
      (set! samples (samples-to-01 samples))) ; naughty!
  (let ([min-val (apply min samples)]
        [max-val (apply max samples)]
        [mean-val (* 1.0 (avg samples))]
        [variance-val (* 1.0 (variance samples))]
        [stddev-val (* 1.0 (stddev samples))])
    (displayln (format "Min: ~a Mean: ~a Max: ~a Variance: ~a Stddev: ~a"
                       min-val mean-val max-val variance-val stddev-val)))
  )


;; Variance of samples
(define (variance samples)
  (let ([s (for/sum ([v samples]) (expt (- v (avg samples)) 2))])
    (/ s (length samples)))
  )


;; Standard deviation of samples
(define (stddev samples)
  (sqrt (variance samples)))

;;
;; (correlation-coefficient1 a1  a2)
;; Returns the correlation-coefficient1 of lists a1 and a2
;;;

;; (define (correlation-coefficient1 a1  a2)
;;   (let ([len1 (length a1)]
;;         [len2 (length a2)])
    
;;     ; Same length
;;     (when (not (= len1 len2))
;;         (error "As have different lengths!"))

;;     (let ([n len1]
;;           [sumX 0]
;;           [sumY 0]
;;           [sumXY 0]
;;           [sumX2 0]
;;           [sumY2 0])
;;       (for ([x a1]
;;             [y a2])
;;         (set! sumX (+ sumX x))
;;         (set! sumY (+ sumY y))        
;;         (set! sumXY (+ sumXY (* x y)))
;;         (set! sumX2 (+ sumX2 (* x x)))
;;         (set! sumY2 (+ sumY2 (* y y)))
;;         )
;;       (let ([numer (- (* n sumXY) (* sumX sumY))]
;;             [denom (sqrt (* (- (* n sumX2) (* sumX sumX))
;;                             (- (* n sumY2) (* sumY sumY))))])
;;         (/ numer denom)))))
;;
(define (correlation-coefficient a1  a2)
  (define (loop a1 a2 n [sumX 0] [sumY 0] [sumXY 0] [sumX2 0] [sumY2 0])
    (let ([len1 (length a1)]
          [len2 (length a2)])
      
      ; Same length
      (when (not (= len1 len2))
        (error "As have different lengths!"))
      
      (if (null? a1)
          ; calculate the correlation-coefficient
          (let* ([numer (- (* n sumXY) (* sumX sumY))]
                 [denom (sqrt (* (- (* n sumX2) (* sumX sumX))
                                 (- (* n sumY2) (* sumY sumY))))])
            (/ numer denom))
          ; else: continue
          (let ([x (first a1)]
                [y (first a2)])
            (loop (rest a1)
                  (rest a2)
                  n
                  (+ sumX x)
                  (+ sumY y)
                  (+ sumXY (* x y))
                  (+ sumX2 (* x x))
                  (+ sumY2 (* y y)))))))

  (loop a1 a2 (length a1))
  ) 


;;;
;;; (hash-sort hash cmp key)
;;;
;; Sort a hash and return a list of the sorted elements.
;; Parameters
;; * h: The hash
;; * cmp: default <
;; * key: default cdr 
;;
(define (hash-sort h [cmp <] [key cdr])
  (sort (hash->list h) cmp #:key key)
  )

;;;
;;; (define (collect lst)
;;; Collect all values and return a hash
;;; 
(define (collect lst)
  (let ([h (make-hash)])
    (for ([e lst])
      (hash-update! h e add1 0)
      )
    h)
  )

;;;
;;; (collect-sort-values-down lst)
;;; Show the occurrences of lst and return a hash sorted on decreasing values 
;;; 
(define (collect-sort-values-down lst)
  (collect-sort lst > cdr)
  )

;;;
;;; (collect-sort-values-down lst)
;;; Show the occurrences of lst and return a hash sorted on increasing keys 
;;; 
(define (collect-sort-keys-up lst)
  (collect-sort lst)
  )

;;;
;;; (collect-sort-values-down lst)
;;; Show the occurrences of lst and return a hash, default sorted on increasing keys 
;;; 
(define (collect-sort lst [cmp <] [key car])
  (let ([h (make-hash)])
    (for ([e lst])
      (hash-update! h e add1 1)
      )
    (sort (hash->list h) cmp #:key key ))
  )


;; (show-freq-1 lst #:exact? [exact? #t])
;; Print the frequences of lst in decreasing order of values
;; Default the probabilities are shown as inexact.
;; To show exact values, change to
;;    #:exact? #t
(define (show-freq-1 lst #:exact? [exact? #f])
  (let* ([c (collect-sort-values-down lst)]
         [total (for/sum ([e c]) (cdr e))]
         [totalf (* 1.0 (for/sum ([e c]) (cdr e)))]
         )
    (for ([e c])
      (displayln (list (car e) ":"
                       (/ (cdr e) (if exact? total totalf))))
      )
    )
  )

#|

  (show-freq lst #:exact? [exact? #t])
  Print the frequences of lst in decreasing order of values and the mean value.
  Default the probabilities and mean are shown as inexact.
  - #:exact?
  To show exact values, change to
    #:exact? #t

  - #:sort-down?: 
  Default sort method is to show the values+probabilities
  in decreasing order of probabilities.
  If #:sort-down #f the order is increasing order of values.

|#
(define (show-freq lst #:exact? [exact? #f] #:sort-down? [sort-down? #t])
  ;;; If lst consists of multi-element then the 'fancy' version (show-freq) cannot be
  ;;; used, so call show-freq-1 instead.
  (if (not (number? (first lst)))
      (show-freq-1 lst #:exact? exact?)
      (let* ([c (if sort-down?
                    (collect-sort-values-down lst)
                    (collect-sort lst))]
             [total (for/sum ([e c]) (cdr e))]
             [totalf (* 1.0 (for/sum ([e c]) (cdr e)))]
             [values-sum 1]
             )
        (for ([e c])
          (let* ([val (car e)]
                 [weight (cdr e)]
                 [p (/ weight (if exact? total totalf))]
                 )
            (set! values-sum (+ values-sum (* val weight)))
            ; (displayln (list val ":" p "t:" (* val p)))
            (displayln (list val ":" p))
            )
          )
        (displayln (list "mean:" (if exact?
                                     (/ values-sum total)
                                     (* 1.0 (/ values-sum total))
                                     ))
                   )
        )
      )
  )


;;
;; (show var val)
;; For print debugging.
;; E.g.
;; (define x 10)
;; (show "x" x)
;; ->
;    x 10
;;
(define (show var val)
  (displayln (format "~a: ~a" var val)))

;;
;; (show2 . args)
;; Just a wrapper for displayln
;;
(define (show2 . args)
  (displayln args))


;;;
;;; (boolean->integer bool)
;;; Convert #t -> 1
;;;         #f -> 0
;;;
(define (boolean->integer bool)
  (if bool 1 0))


;;;
;;; (list-ref2d m i j)
;;; Returns m[i,j]
;;;
(define (list-ref2d m i j)
  (list-ref (list-ref m i) j)
  )

(define (vector-ref2d m i j)
  (vector-ref (vector-ref m i) j)
  )


;;
;; (ref-2d a b i)
;; General approach: a and b are can be either a list or a vector
;; Extract the value in a given the index of i'th value in b
;;
(define (ref-2d a b i)
  (let* ([ix-b (if (list? b) (list-ref b i) (vector-ref b i))]
         [val (if (list? a) (list-ref a ix-b) (vector-ref a ix-b))])
    val
    ))



;;
;; Given var:
;;   Returns the probabilities of a distribution for the variable var
;; Else:
;;   Returns all probabilities as a list with (var prob)
;;
(define (get-probs-var dist (var #f))
  (if var
      (for/first (((v p) (in-dist dist))
                  #:when (eq? var v))
        p)
      (for/list (((v p) (in-dist dist))) (list v p))
      )
  )



;;
;; Draw n elements from a without replacement
;;
(define (draw-without-replacement n a [res '()])
  (if (or (= n 0) (null? a))
      res
      (let* ([d (uniform-draw a)]
             [new-a (remove d a)])
        (draw-without-replacement (sub1 n) new-a (append res (list d))))))

;;
;; Draw - with replacement - n elements from data.
;;
(define (resample n data) (for/list ([i n]) (uniform-draw data)))


;; From hakank_utils.rkt (renamed to list-slice since there's already a slice in Gamble
(define (list-slice lst [offset 0] [n (- (length lst) offset)] )
  (take (drop lst offset) n))


;;
;; (take-last a n)
;; Returns the the last n elements in list a
;;
(define (take-last a n)
  (let* ([len (length a)]
         [i (- len n)])
    (if (>= i 0)
        (list-slice a i)
        a))
  
  )



;;
;; Return the differences of the consecutive values in the list lst
;;
;; The following does not work since it's uneven lengths:
;;   (map - list (rest list))
(define (differences lst)
  (for/list ([i (range 1 (length lst))])
    (- (list-ref lst i) (list-ref lst (sub1 i)))
  ))

;;
;; Count the number of matching value val in the list lst
;;
(define (count-occurrences val lst)
  (count (lambda (v) (= v val)) lst)
  )

(define (count-occurrences-eq val lst)
  (count (lambda (v) (eq? v val)) lst)
  )

;;
;; Count the number of matching sublists sub in the list lst
;;
(define (count-occurrences-sublist sub lst)
  (let ([sub-len (length sub)])
    (for/sum ([i (add1 (- (length lst) sub-len))])      
      (boolean->integer (equal? (list-slice lst i sub-len) sub))
      )
    )
  )

;;
;; (define (index-of-sub a sub)
;; Return the first position that sub-list sub occurs in list a.
;; If there is no such occurrence, return the length of a.
;;
(define (index-of-sub a sub)
  (let* ([sub-len (length sub)]
         [e (for/first ([i (add1 (- (length a) sub-len))]
                        #:when (equal? (list-slice a i sub-len) sub))
              i)])
    (if e e (length a))
    )
  )
   
; (show "first-pos" (first-pos '("h" "h" "h" "h" "h" "t" "t" "h" "t") '("h" "t")))


;;
;; rounds the number v to the (1/c)'th decimal
;;
(define (roundf v c) (* (round (/ v c)) c))

;;
;; (simplex-list lst)
;; (simplex-vector lst)
;; Returns a list or vector with the values translated so the sum is 1
;; 
(define (simplex-list lst)     
  (let ([s (sum lst)])
    (for/list ([v lst]) (/ v s))))

(define (simplex-vector lst)
  (let ([s (sum lst)])
    (for/vector ([v lst]) (/ v s))))


(define (ones-list n val) (build-list n (const val)))
(define (ones-vector n val) (build-vector n (const val)))

(define (rep n v) (ones-list n v))

; Racket already has argmax but it's not the one I want
(define (argmax2 lst)
  (index-of lst (apply max lst))
  )

(define (argmin2 lst)
  (index-of lst (apply min lst))
  )


;;
;; (flip-until-pattern patt [coins '(1 0)]
;; Flip a fair coin until pattern patt occur.
;; Returns the start position of the pattern.
;;
(define (flip-until-pattern patt [coins '(1 0)])
  (define (loop a patt acc)
    (define a-len (length a))
    (define p-len (length patt))
    (cond [(and (>= a-len p-len) (< (index-of-sub a patt) a-len)) acc]
          [else (loop (append a (list (uniform-draw coins))) patt (add1 acc))]))
  
  (loop '() patt 0)
  )

;;
;; (draw-until-pattern patt draw)
;; call draw until pattern pattern is found
;; Returns the start position of the pattern.
;; This is a generalized version of flip-until-pattern
;;
(define (draw-until-pattern patt draw)
  (define (loop a patt acc)
    (let ([a-len (length a)]
          [p-len (length patt)])
      (cond [(and (>= a-len p-len) (< (index-of-sub a patt) a-len)) acc]
            [else (loop ; (append a (list (uniform-draw coins)))
                   (append a (list (draw)))
                   patt
                   (add1 acc))]))
    )
  (loop '() patt 0)
  )

;;
;; Alternative approach using match
;; Seem to be a little faster than draw-until-pattern
;; TODO: Check this more. It does not return the same as draw-until-pattern!
;; (define (draw-until-pattern2 patt draw)
;;   (define (loop a patt acc)
;;     (match a
;;       [(list x y ___ x) acc]
;;       [_ (loop (append a (list (draw)))
;;             patt
;;             (add1 acc))]
;;     ))
;;   (loop '() patt 0)
;;   )


;;
;; (draw-until-patterns patterns draw)
;; Draw from the lambda function draw until any of the pattern occurs.
;; TODO: Check this more!
;;
(define (draw-until-patterns patterns draw)
  (define (loop a acc)
    (let* ([a-len (length a)]
           [matches (for/first ([patt patterns]
                                #:when (and (>= a-len (length patt)) (< (index-of-sub a patt) a-len)))
                      (list patt acc))])
      (cond
        [matches matches]
        [else (loop (append a (list (draw))) (add1 acc))])
      )
    )
  (loop '() 0)
  )
   

;
; (make-tuples n lst)
; Make tuples of size n
; Example:
; n=2 (a b c d e) -> (a b) (b c) (d e)
;
(define (make-tuples n lst)
  (define (loop lst acc)
    (cond
      [(empty? lst) acc]
      [(loop (drop lst n) (append acc (list (take lst n))))]))
  (loop lst '())
  )

;
; (chunks-of lst size)
; Returns a list of all size sized sub lists.
;
; Example:
; > (chunks-of (range 1 10) 4)
; '((1 2 3 4) (2 3 4 5) (3 4 5 6) (4 5 6 7) (5 6 7 8) (6 7 8 9))
;
; (from utils_hakank.rkt)
;
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

;
; (num-runs a)
; Returns the number of runs in the list a
;
(define (num-runs a)
  (define (loop t a acc)
    (if (empty? a)
        (if (eq? t a) acc (add1 acc))
        (let ([first-a (first a)]
              [rest-a (rest a)])
          (loop first-a rest-a (+ (if (eq? t first-a) 0 1) acc)))))

  (if (empty? a)
      0
      (loop (first a) (rest a) 0))
  )

; (factorial n)
(define (factorial n)
  (apply * (range 1 (add1 n))))
