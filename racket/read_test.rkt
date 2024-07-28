#|
  Read test in Racket

  """
  This is one of my standard test when learning a new programming
  language: read a word file and filters the words that match 
  the regular expression
      a.*b.*c..., b.*c.*d.., c.*d.*e..., etc
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(require "utils_hakank.rkt")

(define (make-read-test-regex s)
  (string-append ".*" (apply string (flatten (for/list ([c s])
                                               (list c #\. #\*)
      )))
                 )
  )

(define *eng_wordlist* "words_lower.txt")
(define *swe_wordlist* "sv_spelling_org_utf8.txt")
(define *alpha_swe* "abcdefghijklmnopqrstuvwxyzåäö")
(define *alpha_eng* "abcdefghijklmnopqrstuvwxyz")

(define (read-test [lang "eng"] [len 5] )
  (let ([words (file->lines (if (equal? lang "swe") *swe_wordlist*  *eng_wordlist* ))]
        [alpha (if (equal? lang "swe") *alpha_swe* *alpha_eng*)])
    (writeln (list "num words" (length words)))
    (for/list ([x (chunks-of (string->list alpha) len)])
      (let ([r (regexp (make-read-test-regex x))])
        (writeln r)
        (let ([matched (for/list ([word words]
                                  #:when (regexp-match r word))
                         word)])
          (writeln (list (length matched) matched))
          )
        )
      )
    #t)
  )

(time (read-test))
;;; (time (read-test "swe"))
;;; (time (read-test "swe" 6))
