/*

  Euler problem 50
  """
  The prime 41, can be written as the sum of six consecutive primes:
  41 = 2 + 3 + 5 + 7 + 11 + 13

  This is the longest sum of consecutive primes that adds to a prime 
  below one-hundred.

  The longest sum of consecutive primes below one-thousand that adds 
  to a prime, contains 21 terms, and is equal to 953.

  Which prime, below one-million, can be written as the sum of the 
  most consecutive primes?
  """ 

* [max_sum 997651]
** [max_limit 546]


  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/
   
compile('/home/hakank/Poplib/init.p');

;;; Eratostenes 
define prime_sieve(n);
   lvars bits = inits(n), i, j;
   [% 
      for i from 2 to n do
          if bits(i) = 0 then
              i;
              for j from 2*i by i to n do
                  1 -> bits(j);
              endfor;
          endif;
      endfor
      %];
enddefine;


define prime_sieve2(n);
   lvars bits = inits(n), i, j;
   for i from 2 to n do
       if bits(i) = 0 then
           for j from 2*i by i to n do
               1 -> bits(j);
           endfor;
       endif;
   endfor;
   bits;
enddefine;


define sum(a);
    applist(0,a, nonop +); 
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
define n_primes(n);
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



define is_prime(n);
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
    for i from 3 by 2 to round(sqrt(n))+1 do
        if n mod i = 0 then
            return(false);
        endif;
    endfor;
    return(true);
enddefine;

newmemo(is_prime, 100)->is_prime;


;;;
;;; 3.69s
;;;
define problem50;

    lvars prime_limit = 999999;
    lvars primes = prime_sieve(prime_limit);
    lvars limit = 2, i, p, len,offset, found = 0;
    
    for len from 550 by -1 to 21 do
        if found > 0 then
            quitloop;
        endif;
        for offset from 1 to 550 do
            lvars L = [% for p from offset+1 to offset+len do primes(p) endfor %];
            lvars pp = applist(0, L, nonop +);
            if pp > 0 and pp <= prime_limit and is_prime(pp) then
                pp->found;
                quitloop(2);
            endif;
        endfor;
    endfor;
    found=>

enddefine;

;;;
;;; Too slow
;;;
define problem50b;

    lvars prime_limit = 999999;
    ;;; lvars primes = prime_sieve(prime_limit div round(log(prime_limit)));
    lvars primes = prime_sieve2(prime_limit);
    lvars len = primes.length;
    [len ^len]=>
    
    lvars limit = 21, i, p;
    lvars max_len = 0,
         max_list = [],
         max_sum = 0,
         max_limit = limit;

    for i from 1 to len-max_limit do
        for limit from max_limit to len-max_limit do
            lvars L = [% for p from i to limit do primes(p) endfor %];
            lvars sum = applist(0, L, nonop +);
            ;;;if sum <= prime_limit and is_prime(sum) then
            if sum <= prime_limit and primes(sum) = 1 then                
                if L.length > max_len then
                    L.length->max_len;
                    L->max_list;
                    sum->max_sum;
                    limit->max_limit;
                endif;
            endif;
            if sum > prime_limit then
                quitloop(2);
            endif;
        endfor;
    endfor;

    [max_len ^max_len]==>
    [max_sum ^max_sum]==>
    [max_limit ^max_limit]==>

enddefine;


;;;
;;; 6.37s
;;; 
define problem50c;
    lvars n=1000000;
    lvars primes=n_primes(n);
    lvars found=0;
    lvars j, pp, len=0,offset=0;
    lvars hash = newmapping([], 100000, 0, true);
    lvars p;
    for p in primes do
        1->hash(p);
    endfor;
    for len from 555 by -1 to 21 do
        for offset from 1 to 549 do
            sum([% for j from offset+1 to offset+len do primes(j) endfor %])->pp;
            if pp < 1000000 and hash(pp) then
            ;;; if pp < 1000000 and is_prime(pp) then                
                pp->found;
                quitloop(2);
            endif;
        endfor;
    endfor;
    found=>;
enddefine;

;;;
;;; 0.02s
;;; 
define problem50d;
    lvars n=10000;
    lvars i;
    lvars primes=[2]<>[%
                       for i from 3 by 2 to n do
                           if is_prime(i) then
                               i;
                           endif;
                       endfor%];
         lvars len,j,offset,sum,found=0;
    for len from 550 by -1 to 21 do
        for offset from 1 to 549 do
            0->sum;
            for j from offset+1 to offset + len do
                primes(j)+sum->sum;
                if sum > 1000000 then
                    quitloop(2);
                endif;
            endfor;
            if sum < 1000000 and is_prime(sum) then
                sum->found;
                quitloop(2);
            endif;
        endfor;
    endfor;
    found=>;
enddefine;


;;; 'problem50()'=>
;;; problem50();
;;; timediff()=>;

;;; 'problem50b()'=>
;;; problem50b();
;;; timediff()=>;

;;; 'problem50c()'=>
;;; problem50c();
;;; timediff()=>;

'problem50d()'=>
problem50d();
timediff()=>;
