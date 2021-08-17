/*
  Test of defining operators.
*/

;;; One can define operators like this
;;;; The number after define is the precedence.
;;; See HELP define
define 7 a *** b ->c;
   a+b+1->c;
enddefine;

2 *** 3=>;
;;; ** 6
