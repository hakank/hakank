/*

  Problem 10
  """
  The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

  Find the sum of all the primes below two million.
  """

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/


*/
compile('/home/hakank/Poplib/init.p');

;;;
;;; is_prime(n)
;;;
;;; Returns true if n is a primes else false;
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

;;;
;;; sum(a)
;;;
;;; Returns the sum in vector/array a.
;;;
define sum(a);
    ;;; lvars i, s=0;
    ;;; for i in a do
    ;;;     s+i->s;
    ;;; endfor;
    ;;; s;
    applist(2,a, nonop +);
enddefine;

;;;
;;; using is_prime/1
;;;
;;; 3.07s
;;;
define problem10();
    lvars res = 2;
    lvars i;
    for i from 3 by 2 to 2000000-1 do
        if is_prime(i) then
            res + i -> res;
        endif;
    endfor;
    res =>

enddefine;

;;;
;;; Using is_prime and sum
;;; 3.09s
;;;
define problem10b();
    lvars i;
    sum([% 
            for i from 3 by 2 to 2000000-1 do
                if is_prime(i) then
                    i;
                endif;
            endfor;%])=>;    
enddefine;

;;;
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
;;; Using a sieve
;;;
;;; 0.26s;
define problem10c();
    lvars i, j, n=2000000,sum=0,s=initshortvec(n);
    ;;; the sieve
    for i from 2 to round(sqrt(n))+1 do
        for j from i*i by i to n do
            1->s(j);
        endfor;
    endfor;
    ;;; sum the primes
    for i from 2 to n do
        if s(i) = 0 then
            sum+i->sum;            
        endif;
        
    endfor;
    sum=>;
enddefine;

;;;
;;; Using primes/1 and sum/1.
;;;
;;; 0.26s
;;;
define problem10d();
    sum(primes(2000000)); 
enddefine;

;;; 'problem10()'=>
;;; problem10();
;;; timediff()=>

;;; 'problem10b()'=>
;;; problem10b();
;;; timediff()=>

'problem10c()'=>;
problem10c()=>;
timediff()=>

;;; 'problem10d()'=>;
;;; problem10d()=>;
;;; timediff()=>
