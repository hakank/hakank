/*
   Tue Jul 28 16:15:46 2009/hakank@bonetmail.com

   Sum squares.

   Compare with http://rosettacode.org/wiki/Sum_of_squares#Pop11
   """
   Write a program to find the sum of squares of a numeric vector. 
   The program should work on a zero-length vector (with an answer of 0). 
   """

*/

define sumsquares_array(L)->s;
    lvars i, s = 0;
    for i in L do s+i*i->s endfor;
enddefine;

define sumsquares_vector(A)->s;
    lvars i, s = 0;
    for i to A.length do s+A(i)**2->s endfor;
enddefine;


'testing array'=>;
vars L, s,i ;
[1 2 3 4 5] -> L; ;;; this is not a numeric vector...
0->s; for i in L do s+i*i->s endfor;
s=>;

sumsquares_array(L)=>
sumsquares_array([])=>

;;; ah, this don't work. {...} is a vector
;;; sumsquares({1 2 3 4 5})=>

'testing vector'=>;
vars A;
{1 2 3 4 5}->A;
sumsquares_vector(A)=>;
sumsquares_vector({})=>;