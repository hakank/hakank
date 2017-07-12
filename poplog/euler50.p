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


  This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com).
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

newmemo(is_prime, 100000)->is_prime;

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
                quitloop;
            endif;
        endfor;
    endfor;
    found=>

enddefine;

;;; Too slow
define problem50b;

    lvars prime_limit = 999999;
    lvars primes = prime_sieve(prime_limit div round(log(prime_limit)));
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
            if sum <= prime_limit and is_prime(sum) then
                if L.length > max_len then
                    [prime ^sum ^(L.length)  i ^i max_limit ^max_limit]=>
                    L.length->max_len;
                    L->max_list;
                    sum->max_sum;
                    limit->max_limit;
                    timediff()=>
                endif;
            endif;
            if sum > prime_limit then
                nextloop(2);
            endif;
        endfor;
    endfor;

    [max_len ^max_len]=>
    [max_list ^max_list]=>
    [max_sum ^max_sum]=>
    [max_limit ^max_limit]=>

enddefine;


'problem50()'=>
problem50();
