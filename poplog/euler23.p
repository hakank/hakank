/*

  Euler problem 23
  """
  A perfect number is a number for which the sum of its proper divisors 
  is exactly equal to the number. For example, the sum of the proper divisors 
  of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
 
  A number n is called deficient if the sum of its proper divisors is less than 
  n and it is called abundant if this sum exceeds n.
 
  As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number 
  that can be written as the sum of two abundant numbers is 24. By mathematical 
  analysis, it can be shown that all integers greater than 28123 can be written 
  as the sum of two abundant numbers. However, this upper limit cannot be reduced 
  any further by analysis even though it is known that the greatest number that 
  cannot be expressed as the sum of two abundant numbers is less than this limit.
 
  Find the sum of all the positive integers which cannot be written as the sum of 
  two abundant numbers.

  """ 

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/

compile('/home/hakank/Poplib/init.p');

define sum_divisors(n);
    lvars s = 0;
    lvars i;
    for i from 1 to round(n/2) do
        if n mod i = 0 then
            s + i -> s;
        endif;
    endfor;
    return(s);
enddefine;


;;;
;;; This is much faster, by a factor 10 or so
;;;
define sum_divisors2(n);
    lvars d = floor(sqrt(n)),
          sum = 1,
          i;
    for i from 2 to d do
        if n mod i = 0 then
            sum+i->sum;
            if i /= n div i then
                sum+(n div i)->sum;
            endif;
        endif;
    endfor;

    sum;
enddefine;

;;;
;;; about 1.12s
;;;
define problem23; 
    lvars n = 28123; 
    lvars a;

    ;;; collect the abundant numbers
    lvars abundant = [% for a from 1 to n do 
                                 if sum_divisors2(a) > a then
                                     a;
                                 endif;
                             endfor %];

    lvars hash = newmapping([], 10000, 0, true);
    lvars b;
    for a in_list abundant do 
        for b in_list abundant do 
            if a >= b and a+b <= n then
                1->hash(a+b);             
            else 
                nextloop(2);
            endif;
            
        endfor;
    endfor;

    lvars sum = 0;
    for a from 1 to n do
        if hash(a) = 0 then
            sum+a->sum;
        endif;
    endfor;

    sum=>

enddefine;


;;;
;;; Using vector for the sums
;;;
;;; This is the fastest 0.66s
;;;
define problem23b; 
    lvars n = 28123; 
    lvars a;

    lvars abundant = [% for a from 1 to n do 
                                 if sum_divisors2(a) > a then
                                     a;
                                 endif;
                             endfor %];

    lvars vec = initshortvec(n);
    lvars b;
    fast_for a in abundant do 
        fast_for b in abundant do 
             if a >= b and a+b <= n then
                 1->vec(a+b);
             else
                 nextloop(2);
             endif;           
        endfast_for;
    endfast_for;

    lvars sum = 0;
    for a from 1 to n do
        if vec(a) /= 1 then
            sum+a->sum;
        endif;
    endfor;

    sum=>

enddefine;


;;;
;;; Using vector for abundant
;;; Slowest version: 1.45s
;;;
define problem23c; 
    lvars n = 28123; 
    lvars a;

    lvars abundant = {% for a from 1 to n do 
                                 if sum_divisors2(a) > a then
                                     a;
                                 endif;
                             endfor %};

     lvars vec = initshortvec(n);
     lvars len = abundant.length;
     lvars i,j;
     for i from 1 to len do 
         for j from i to len do
             lvars c = abundant(i)+abundant(j);
             if c <= n then 
                 1->vec(c);             
             endif;
         endfor;
     endfor;

    lvars sum = 0;
    for a from 1 to n do
        if vec(a) /= 1 then
            sum+a->sum;
        endif;
    endfor;

    sum=>

enddefine;

vars t;
'problem23()'=>
problem23();

timediff()->t;
t=>

'problem23b()'=>
problem23b();
timediff()->t;
t=>


'problem23c()'=>
problem23c();

timediff()->t;
t=>

