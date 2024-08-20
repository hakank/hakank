#lang racket


(define *answers*
  (make-hash
   '(
     (1 . 233168)
     (2 . 4613732)
     (3 . 6857)
     (4 . 906609)
     (5 . 232792560)
     (6 . 25164150)
     (7 . 104743)
     (8 . 40824)
     (9 . 31875000)
     (10 . 142913828922)
     (11 . 70600674)
     (12 . 76576500)
     (13 . 5537376230)
     (14 . 837799)
     (15 . 137846528820)
     (16 . 1366)
     (17 . 21124)
     (18 . 1074)
     (19 . 171)
     (20 . 648)
     (21 . 31626)
     (22 . 871198282)
     (23 . 4179871)
     (24 . 2783915460)
     (25 . 4782)
     (26 . 983)
     (27 . -59231)
     (28 . 669171001)
     (29 . 9183)
     (30 . 443839)
     (31 . 73682)
     (32 . 45228)
     (33 . 100)
     (34 . 40730)
     (35 . 55)
     (36 . 872187)
     (37 . 748317)
     (38 . 932718654)
     (39 . 840)
     (40 . 210)
     (41 . 7652413)
     (42 . 162)
     (43 . 16695334890)
     (44 . 5482660)
     (45 . 1533776805)
     (46 . 5777)
     (47 . 134043)
     (48 . 9110846700)
     (49 . 296962999629)
     (50 . 997651)
    ))
  )

(define (run-euler)
  (let ([re #px"(?s:#<procedure:euler(\\d+)[^>]+?>\n(.+?)\ncpu time: (\\d+) .+?\n?)"]
        [problem-re #px"#<procedure:euler(\\d+).*?>"]
        [total-time 0]
        [times (make-hash)]
        [failures '()]
        [output (with-output-to-string (lambda () (system "racket run_euler.rkt")))])
    (display output)
    (for ([match (in-list (regexp-match* re output))])
      (let* ([lst (string-split match "\n")]
             [problem (string->number (second (regexp-match problem-re (first lst))))]
             [correct-answer (hash-ref *answers* problem)]
             [answer (string->number (second lst))]
             [is-correct (= correct-answer answer)]
             [cpu-time (string->number (third (string-split (third lst) " ")))])
        (displayln (list "problem" problem "correct-answer" correct-answer "answer" answer "cpu-time" cpu-time "is-correct" is-correct ))
        (if is-correct
            (begin
              (hash-set! times problem cpu-time)
              (set! total-time (+ total-time cpu-time))
              )
            (begin
              (writeln (format "problem ~a is not correct. Was ~a, but should be ~a" problem correct-answer answer ))
              (set! failures (cons problem failures))
              ))
        )
      )
    (newline)
    (displayln "Problem in order of problem number:")    
    (for ([key (in-hash-keys times)])
      (displayln (format "Problem ~a: ~as" key (/ (hash-ref times key) 1000.0))))
    (newline)
    (displayln "Problem in descreasing order of solution time:")
    (for ([kv (sort (hash->list times) > #:key cdr)])
      (let ([problem (car kv)]
            [cpu-time (cdr kv)])
        (displayln (format "Problem ~a: ~as" problem (/ cpu-time 1000.0)))
        )
      )
    (newline)
    (if (> (length failures) 0)
        (displayln (format "Failures: ~a" (reverse failures)))
        (displayln "All OK!")
        )
    (displayln (format "Total time: ~as" (/ total-time 1000.0)))
    (newline)
    )
  )

;;; Checking each program
;; "Testing problem 1"
;; #<procedure:euler1a>
;; 233168
;; cpu time: 0 real time: 0 gc time: 0
(define (run-euler2)
  (define re #px"(?s:#<procedure:euler\\d[^>]+?>\n(.+?)\ncpu time: (\\d+) .+?\n?)")
  (define total-time 0)
  (define times (make-hash)) 
  (define invalid '())
  (for ([p (range 1 51)])
    (newline)
    (writeln (format "Testing problem ~a" p))
    ;;; (writeln (list "answer" (hash-ref *answers* p)))
    (let* ([program (format "racket euler~a.rkt" p)]
           [correct-answer  (hash-ref *answers* p)]
           [output (with-output-to-string (lambda () (system program)))]
           [match (regexp-match re output)]
           [answer (second match)]
           [cpu-time (string->number(third match))]
           [correct (if (equal? answer (number->string correct-answer)) #t #f)]
           )
      ;;; (display output)
      (writeln (list "answer" answer "cpu-time" cpu-time "correct" correct ))
      (if (eq? correct #t)
          (begin
            (set! total-time (+ total-time cpu-time))
            (hash-set! times p cpu-time)
            )
          (begin
            (writeln (list "INCORRECT! should be " correct-answer))
            (set! invalid (cons p invalid))
            )
          )
      )
    )
  (writeln (list "total-time" total-time))
  (writeln (list "times" times))
  (writeln (list "invalid" invalid))  
  )

;; (define (test)
;;   (define re #px"(?s:#<procedure:euler\\d[^>]+?>\n(.+?)\ncpu time: (\\d+) .+?\n?)")
;;   (let ([output "\"Testing problem 2\"])
;; #<procedure:euler2e>
;; 4613732
;; cpu time: 0 real time: 0 gc time: 0"])
;;     (displayln (list "output" output))
;;     ;;; (define re #px"(?s:.+?cpu time: (\\d+) .+?)")
;;     (let ({m (regexp-match re output)})
;;       (writeln (list "m" m)))
;;     (for ([match (in-list (regexp-match re output))])
;;       (writeln (list "match" match))
;;       )
;;     )
;;   )

;;; (test)

(time (run-euler))

;;; (time (run-euler2))
