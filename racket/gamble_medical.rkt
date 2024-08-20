#| 

  Medical in Racket Gamble.

  From Church

  Result:
  '("cough" #f "fever" #f "chest-pain" #f "shortness-of-breath" #f)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

;;; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define lung-cancer (flip 0.01))
(define TB (flip 0.005))
(define stomach-flu (flip 0.1))
(define cold (flip 0.2))
(define other (flip 0.1))

(define cough
  (or (and cold (flip 0.5))
      (and lung-cancer (flip 0.3))
      (and TB (flip 0.7))
      (and other (flip 0.01))))


(define fever
  (or (and cold (flip 0.3))
      (and stomach-flu (flip 0.5))
      (and TB (flip 0.1))
      (and other (flip 0.01))))


(define chest-pain
  (or (and lung-cancer (flip 0.5))
      (and TB (flip 0.5))
      (and other (flip 0.01))))

(define shortness-of-breath
  (or (and lung-cancer (flip 0.5))
      (and TB (flip 0.2))
      (and other (flip 0.01))))

(list "cough" cough
      "fever" fever
      "chest-pain" chest-pain
      "shortness-of-breath" shortness-of-breath)
