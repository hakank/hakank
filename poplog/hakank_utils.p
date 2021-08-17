/*

  Some utilities in Pop-11.

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com)
  See also my Pop-11/Poplog page: http://www.hakank.org/poplog/


*/
compile('/home/hakank/Poplib/init.p');

;;;
;;; sum_list(a)
;;;
;;; Returns the sums of vector/array a.
;;;
define sum_list(a);
    applist(0,a, nonop +); 
enddefine;

;;;
;;; prod_list(a)
;;;
;;; Returns the product of vector/array a.
;;;
define prod_list(a);
    applist(1,a, nonop *); 
enddefine;


;;;
;;; even(n)
;;;
;;; Returns true if num is even.
;;;
define even(n);
  if n mod 2 = 0 then 
      return(true);
  endif;
  return(false);
enddefine;

;;;
;;; odd(num)
;;;
;;; Returns true if num is odd.
;;;
define odd(n);
  if n mod 2 = 1 then 
      return(true);
  endif;
  return(false);
enddefine;

;;;
;;; fib(n)
;;;
;;; Returns the n'th Fibonacci number.
;;; Note it is memoised.
;;;
define fib(n);
  if n <= 1 then 
     return(1);
  endif;
  return(fib(n - 1) + fib(n -2));
enddefine;

newmemo(fib,200) -> fib;



;;;
;;; is_prime(n)
;;;
;;; Returns true if n is a prime.
;;;
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

newmemo(is_prime,2000) -> is_prime;


;;;
;;; is_prime2(n)
;;;
;;; Returns true if n is a prime.
;;;
define is_prime2(n);
    lvars i;
    if n <= 1 then
        return(false);
    endif;
    if n = 2 or n = 3 then
        return(true);
    endif;
    if n mod 2 = 0 then
        return(false);
    endif;
    3->i;
    while n mod i > 0 and i <= round(sqrt(n))+1 do
        if n mod i = 0 then
            return(false);
        endif;
        ;;; next_prime(i)->i;
        i+2->i;
    endwhile;
    return(true);
enddefine;


;;;
;;; divisors(n)
;;;
;;; Returns all the divisors of n (including 1 and n)
;;;
define divisors(n);
   lvars i;
   [1 %for i from 2 to round(n/2) do if (n mod i = 0) then i endif endfor% ^n]

enddefine;


;;; primes(n)
;;;
;;; Returns the primes from 2..n.
;;;
;;; primes(100)
;;; ** [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89
;;;	97]
;;;
define primes(n);
    lvars i, j, p=[], s=initshortvec(n);
    ;;; the sieve
    for i from 2 to round(sqrt(n))+1 do
        for j from i*i by i to n do
            1->s(j);
        endfor;
    endfor;
    
    [% for i from 2 to n do
        if s(i) = 0 then
            i;
        endif;
    endfor%];
enddefine;


;;;
;;; next_prime(n)
;;;
;;; Returns the next prime of n.
;;;
define next_prime(n);
    lvars m=n+2;
    while not(is_prime2(m)) do
        m+2->m;
    endwhile;
    m;
enddefine;

;;;
;;; factors(n)
;;;
;;; Returns the factors of n.
;;;
define factors(n);
    [factors ^n]=>;
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
