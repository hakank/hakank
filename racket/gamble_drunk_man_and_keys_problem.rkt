#| 

  Drunk man and keys problem in Racket Gamble.

  https://medium.com/recreational-maths/drunk-man-and-keys-problem-37df70a0cf15
  """
  The drunk man and keys problem is a simple but interesting probability 
  problem.

  A drunk man, returning home, is trying to open his house door with a bunch 
  of keys. There are 10 keys in the bunch, but only 1 fits the door. He 
  selects a key at random. If it fits the door he lets himself in, but if it 
  doesn’t fit he tries again. Because he is drunk, he doesn’t remember which 
  keys he has already tried, so he makes a totally random choice from the 
  10 keys each time.

  The question is, on which attempt is he most likely to get in?

  Some people think that the answer is 5 (because that is half of 10). 
  Others say that there is no right answer because every time he tries he 
  has exactly the same chance of picking the right key.

  These might seem like reasonable answers, but they are answers to 
  the wrong question.

  In fact, he is most likely to get in on his first attempt!
  """
  
  Using (show-freq (repeat sample 10000))
(1 : 0.09827278141751042)
(2 : 0.08775064522533253)
(3 : 0.08278737343656939)
(4 : 0.07772483621203097)
(5 : 0.06670637284097677)
(6 : 0.06075044669446099)
(7 : 0.051320230295810995)
(8 : 0.046158427635497316)
(9 : 0.042684137383363116)
(10 : 0.03643041492952154)
(11 : 0.03523922970021839)
(12 : 0.03126861226920786)
(13 : 0.02968036529680365)
(15 : 0.023922970021838397)
(14 : 0.022036926742108397)
(16 : 0.018959698233075244)
(17 : 0.01846337105419893)
(19 : 0.01568393885249156)
(20 : 0.01498908080206472)
(18 : 0.014889815366289458)
(21 : 0.01171332142148104)
(22 : 0.01042287075640262)
(23 : 0.009827278141751042)
(25 : 0.009529481834425254)
(24 : 0.009033154655548938)
(27 : 0.00784196942624578)
(26 : 0.007345642247369466)
(28 : 0.005658129839189994)
(29 : 0.005558864403414731)
(30 : 0.0045662100456621)
(31 : 0.003772086559459996)
(32 : 0.0031764939448084176)
(37 : 0.0031764939448084176)
(35 : 0.0029779630732578916)
(33 : 0.0026801667659321023)
(39 : 0.0025809013301568394)
(36 : 0.0023823704586063135)
(38 : 0.00228310502283105)
(34 : 0.002183839587055787)
(40 : 0.001786777843954735)
(43 : 0.0013897161008536827)
(42 : 0.0011911852293031567)
(41 : 0.0010919197935278936)
(44 : 0.0010919197935278936)
(47 : 0.0009926543577526306)
(46 : 0.0008933889219773674)
(51 : 0.0007941234862021044)
(45 : 0.0006948580504268413)
(48 : 0.0006948580504268413)
(49 : 0.0006948580504268413)
(50 : 0.0005955926146515784)
(56 : 0.0003970617431010522)
(61 : 0.0003970617431010522)
(52 : 0.0002977963073257892)
(53 : 0.0002977963073257892)
(54 : 0.0002977963073257892)
(55 : 0.0002977963073257892)
(63 : 0.0002977963073257892)
(70 : 0.0002977963073257892)
(77 : 0.0002977963073257892)
(57 : 0.0001985308715505261)
(58 : 0.0001985308715505261)
(60 : 0.0001985308715505261)
(62 : 0.0001985308715505261)
(64 : 0.0001985308715505261)
(65 : 0.0001985308715505261)
(66 : 0.0001985308715505261)
(69 : 0.0001985308715505261)
(71 : 0.0001985308715505261)
(75 : 0.0001985308715505261)
(76 : 0.0001985308715505261)
(81 : 0.0001985308715505261)
(87 : 0.0001985308715505261)
(96 : 0.0001985308715505261)
(mean: 10.0146)
Min: 1 Mean: 10.0208 Max: 79 Variance: 86.20856736 Stddev: 9.2848568841959
Credible interval (0.84): 1..18

  Note: Perhaps length 1 (i.e. success at the first trial) is "most likely", but it's not 
        by much over - say - length 2. But, yes, 1 is the most likely.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

;;; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define drunk
  (rejection-sampler

   (define n 10)
   (define max-tries 100)
   (define (f i a)
     (if (> i max-tries)
         (list a false)
         (begin
           (let ([t (discrete-uniform n)])
             (if (= t 0)
                 (list (cons t a) true)
                 (f (add1 i) (cons t a)))
             )
           ))
     )

   (define res (f 0 '()))
   (define a (car res))
   (define success (cadr res))
   (define len (length a))
   (define p (and success (= (first a) 0 )))

   ; (displayln (list "a" a "success" success))
   
   (observe/fail p) ; the drunk succeeded
   
   ; (list len) ; for show-marginals
   len
   )
  )

;;; (sampler->discrete-dist drunk 10000)

; (show-freq (repeat drunk 10000))
(show-model drunk #:num-samples 10000)
; (show-marginals drunk (list "len") #:num-samples 10000)
(newline)
; (displayln (list "mean:" (exact->inexact (sampler->mean drunk 10000))))


; Nope, enumerate is too slow for max-tries 100
;; (enumerate
;;  #:limit 1e-1
;;  (define n 10)
;;  (define max-tries 100)
;;  (define (f i a)
;;    (if (> i max-tries)
;;        (list a false)
;;        (begin
;;          (let ([t (discrete-uniform n)])
;;            (if (= t 0)
;;                (list (cons t a) true)
;;                (f (add1 i) (cons t a)))
;;            )
;;          ))
;;      )
 
;;  (define res (f 0 '()))
;;  (define a (car res))
;;  (define success (cadr res))
;;  (define len (length a))
;;  (define p (and success (= (first a) 0 )))
 
;;  (displayln (list "a" a "success" success))
;;  ; len
;;  (observe/fail p)
;;  ;; (list p len)
;;  ; p
;;  len
;;  )


