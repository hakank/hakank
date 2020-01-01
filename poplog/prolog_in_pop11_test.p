/*

   Test of using Prolog in Pop-11.

   Run as 
     pop11 prolog_in_pop11_test.p

   See 
     * PLOGHELP PLOGTOPOP

   This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com
   See also my Pop-11/Poplog page: http://www.hakank.org/poplog/

*/

uses ploginpop;

;;;
;;; 
'\nAppend test:'=>;
vars x,y;
plogwhile |< append(?x, ?y, [a, b, c, d, e]) >| do
  [^x ^y]=>;
endplogwhile;

;;;
;;; alternative version where we assign the expression to 
;;; the variable g
;;;
'\n\nAlternative version:'=>;
vars g;
vars list1 = [a b c d e]; ;;; as a variable in append/3
|< append(?x, ?y, ^list1) >| -> g;
plogwhile g do
  [^x ^y]=>;
endplogwhile;


'\n\nPermutations:'=>;
;;;
;;; This code is from
;;; http://www.cogs.susx.ac.uk/users/davidy/poplog/permutations.p
;;; 

;;; Define the Prolog clauses for permutation
prolog_compile(stringin('                                          \
delete(A, [A|L], L).                                               \
delete(A, [X|L1], [X|L2]) :- delete(A, L1, L2).                    \
permutation([], []).                                               \
permutation(L1, [A|L2]) :- delete(A, L1, L3), permutation(L3, L2). \
'));

;;;
;;; Print all permutions of the list [a b c d]
;;;
vars list = [a b c d];
vars l2;
vars c = 0; ;;; counter
plogwhile |< permutation(^list, ?l2) >| do
  l2=>;
  c + 1 -> c;
endplogwhile;

[It was ^c occurrences]=>;
