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

(define (multinomial xs ws)
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
            (displayln "Percentiles::")
            (if (list? show-percentiles?)
                (show-percentiles this-sample show-percentiles?)
                (show-percentiles this-sample)                  
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

  Note: exact values are handed as symbolic values they are distinct.
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
(define *default-percentile-ps* (list 0.01 0.1 0.025 0.25 0.5 0.75 0.84 0.9 0.975 0.99 0.999))
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

(define (show-percentiles values [ps *default-percentile-ps*])
  (for ([p (percentiles values ps)])
    (displayln p)
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
      (hash-update! h e add1 1)
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
