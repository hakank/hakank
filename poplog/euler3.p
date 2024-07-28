/*

  Problem 3
  http://projecteuler.net/index.php?section=problems&id=3
  """
  The prime factors of 13195 are 5, 7, 13 and 29.

  What is the largest prime factor of the number 600851475143 ?
  """

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/


*/
;compile('/home/hakank/Poplib/init.p');


define divisors(n);
   lvars i;
   [1 %for i from 2 to round(n/2) do if (n mod i = 0) then i endif endfor% ^n]

enddefine;


define is_prime(n);
    lvars i;
    if n = 2 or n = 3 then
        return(true);
    endif;
    if n mod 2 = 0 then
        return(false);
    endif;
    for i from 3 by 2 to round(sqrt(n))+1 do
        if n mod i = 0 then
            return(false);
        endif;
    endfor;
    return(true);
enddefine;

;;;
;;; factors(n)
;;;
;;; Returns the factors of n.
;;;
define factors(n);
    lvars m = n;

    ;;; if is_prime(n) then 
    ;;;    return([]);
    ;;; endif;
    lvars i,t;
    [% 
       while m mod 2 = 0 do
           2;
           m div 2 -> m;
       endwhile;
       3->t;
       while m > 1 and t < 1+sqrt(m) do
           if m mod t = 0 then
               while m mod t = 0 do
                   t;
                   m div t -> m;
               endwhile;
           endif;
           t+2->t;
           ;;;next_prime(t)->t; ;;; test
       endwhile;
       if m > 1 then
           m;
       endif;
     %];
enddefine;




;;;
;;; Simpler version: loop until the last prime is detected
;;;
define problem3a();

    lvars last;
    lvars n = 600851475143;
    lvars i;
    for i from 2 to round(sqrt(n)) do 
        if n mod i = 0 and is_prime(i) then 
            i -> last 
        endif 
    endfor;
    last=>

enddefine;

define problem3b();
   lvars n = 600851475143;
   lvars f;
   factors(n) -> f;
   last(f)=>
enddefine;

define problem3c();
   factors(600851475143).last
enddefine;


;;; 'problem3a()'=>
;;; problem3a();
;;; timediff()=>;

;;; 'problem3b()'=>
;;; problem3b()=>;
;;; timediff()=>;

'problem3c()'=>
problem3c()=>;
timediff()=>;