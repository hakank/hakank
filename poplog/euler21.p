/*

  Euler problem 21
  """
  Let d(n) be defined as the sum of proper divisors of n (numbers less 
  than n which divide evenly into n).
  If d(a) = b and d(b) = a, where a /= b, then a and b are an amicable 
  pair and each of a and b are called amicable numbers.
  
  For example, the proper divisors of 220 are 
  1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. 
  The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
  
  Evaluate the sum of all the amicable numbers under 10000.
  """ 


  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/
compile('/home/hakank/Poplib/init.p');

vars hash = newmapping([], 100, 0, true);

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
;;; Using a hash table is slightly faster than 
;;; is_amicable1: it takes 1.6s
;;;
define is_amicable(n);
    lvars a,b;
    lvars hn=hash(n);
    if hn > 0 then 
        hn->a;
    else 
        sum_divisors(n)->a;
        a->hash(n);
    endif;

    lvars ha=hash(a);
    if ha > 0 then 
        ha->b;
    else 
        sum_divisors(a)->b;
        b->hash(a);
    endif;
        
    if a = b then 
        return(false);
    endif;

    if a /= b and b = n then
        return(true);
    else
        return(false);
    endif;

enddefine;

;;;
;;; Brute force is slightly slower, 
;;; though fast enough: Using it takes 2.2s.
;;;
define is_amicable1(n);
    lvars a = sum_divisors(n);
    lvars b = sum_divisors(a);

    if a == b then 
        return(false);
    endif;

    if a /= b and b == n then
        return(true);
    else
        return(false);
    endif;

enddefine;


define problem21();
    lvars i;
    lvars sum=0;
    for i from 1 to 10000-1 do
        if is_amicable(i) then
            sum+i->sum;
        endif;
    endfor;

    sum=>;

enddefine;

'problem21()'=>
problem21();


