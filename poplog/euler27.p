/*

  Euler problem 27
  """
  Euler published the remarkable quadratic formula:
 
  n^2 + n + 41
 
  It turns out that the formula will produce 40 primes for the consecutive values 
  n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 
  41, and certainly when n = 41, 41^2 + 41 + 41 is clearly divisible by 41.
 
  Using computers, the incredible formula  n^2 − 79n + 1601 was discovered, which 
  produces 80 primes for the consecutive values n = 0 to 79. The product of the 
  coefficients, −79 and 1601, is −126479.
 
  Considering quadratics of the form:
 
      n^2 + an + b, where |a| < 1000 and |b| < 1000
 
      where |n| is the modulus/absolute value of n
      e.g. |11| = 11 and |−4| = 4
 
  Find the product of the coefficients, a and b, for the quadratic 
  expression that produces the maximum number of primes for consecutive 
  values of n, starting with n = 0.
  """ 

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/

compile('/home/hakank/Poplib/init.p');


define is_prime(n);
    if n = 2 or n = 3 then
        return(true);
    endif;
    if n mod 2 = 0 then
        return(false);
    endif;
    lvars i;
    for i from 3 by 2 to round(sqrt(n))+1 do
        if n mod i = 0 then
            return(false);
        endif;
    endfor;
    return(true);
enddefine;

;;;
;;; This speeds up things from 1.54s to 0.718s
;;;
newmemo(is_prime, 200000) -> is_prime;


define p(a,b);
    lvars n=0;
    while true do
        lvars pp=n**2 + a*n + b;
        if pp <= 1 or not(is_prime(pp)) then
            quitloop();
        else
            n+1->n;
        endif;
    endwhile;

    n+1;
enddefine;

define problem27;
    lvars t=999, bestLen=0;
    lvars besta=0, bestb=0;
    lvars a,b;
    for a from -t to t do
        for b from -t to t do
            lvars len = p(a,b);
            if len > bestLen then
                len->bestLen;
                a->besta;
                b->bestb;
            endif;
        endfor;
    endfor;

    [besta ^besta bestb ^bestb bestLen ^bestLen answer: ^(besta*bestb)]=>;

enddefine;


'problem27()'=>
problem27();


