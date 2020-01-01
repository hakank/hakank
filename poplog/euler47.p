/*

  Euler problem 47
  """
 The first two consecutive numbers to have two distinct prime factors are:
 
  14 = 2 x 7
  15 = 3 x 5
 
  The first three consecutive numbers to have three distinct 
  prime factors are:
 
  644 = 2^2 x 7 x 23
  645 = 3 x 5 x 43
  646 = 2 x 17 x 19.
 
  Find the first four consecutive integers to have four distinct primes 
  factors. What is the first of these numbers?
  """ 

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/

compile('/home/hakank/Poplib/init.p');

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
;;; distinct factors of  ^n
;;;
define distinct_factors(n);
    lvars ll = [];

    if is_prime(n) then 
        return([]);
    endif;

    if n mod 2 = 0 then
        [2]->ll;
        while n mod 2 = 0 then
            n div 2 -> n;
        endwhile;
    endif;
        
    lvars i;
    for i from 3 by 2 to round(n/2) do
        if n mod i = 0 then
            ll<>[^i]->ll;
            while n mod i = 0 then
                n div i -> n;
            endwhile;
        endif;
        if n = 1 then quitloop endif;
    endfor;

    ll;

enddefine;


;;; Slow: 13.8s
define problem47;

    lvars n = 2;
    lvars L = [];
    lvars limit = 4;

    while L.length < limit do

        if is_prime(n) or distinct_factors(n).length /= limit then
            n+1->n;
            []->L;
            nextloop; 
        else
            L<>[^n]->L;
            n+1->n;
        endif;

    endwhile;

    L=>
    L(1)=>

enddefine;


;;; Much faster: 0.28s
define problem47b;

    lvars maxn = 1000000;
    lvars f = initintvec(maxn);

    lvars i,j;
    for i from 2 to maxn-1 do
        if f(i) = 0 then
            for j from 2*i by i to maxn-1 do
                1+f(j)->f(j);
            endfor;
        endif;
    endfor;

    lvars goal = [4 4 4 4];
    for i from 2 to maxn-3 do
        if [% for j from i to i+3 do f(j) endfor;%] = goal then
            i=>
            quitloop;
        endif;
    endfor;

enddefine;


;;; 'problem47()'=>
;;; problem47();
;;; timediff()=>

'problem47b()'=>
problem47b();
timediff()=>

