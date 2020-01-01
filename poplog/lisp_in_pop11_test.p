/*

  Lisp in Pop-11.
 
  See 
    * HELP POPINLISP
  

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com
  See also my Pop-11/Poplog page: http://www.hakank.org/poplog/


*/
uses clisp;

;;; Note: last argument is number of arguments to the Lisp function
@CAR([a b c], 1) =>
;;;    ** a

@MEMBER([b], [[a] [b] [c]], @:TEST, @EQUAL, 4) =>
;;;    ** [[b] [c]]

@MEMBER("b", [a b c], 2) =>

@+(1, 2, 3, 4, 5, 5) =>
;;;     ** 15

@EXPT(2, 120,2)=>
@-(@EXPT(2, 120,2), 1, 2)=>
