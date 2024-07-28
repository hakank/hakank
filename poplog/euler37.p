/*

  Euler problem 37
  """
  The number 3797 has an interesting property. Being prime itself, it is possible to 
  continuously remove digits from left to right, and remain prime at each stage: 
  3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.
  
  Find the sum of the only eleven primes that are both truncatable from left to right 
  and right to left.
  
  NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

  """ 

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/

compile('/home/hakank/Poplib/init.p');

define is_prime(n);
    lvars i;

    if n = 1 then return(false); endif;

    if n = 2 or n = 3 then
        return(true);
    endif;
    if n mod 2 = 0 then
        return(false);
    endif;
    fast_for i from 3 by 2 to round(sqrt(n))+1 do
        if n mod i = 0 then
            return(false);
        endif;
    endfast_for;
    return(true);
enddefine;

newmemo(is_prime,100)->is_prime;

define check2(n);
   lvars L = n.unpackitem;
   lvars i;
   fast_for i on L do 
       if not(is_prime(i.packitem)) then
           return(false);
       endif;
   endfast_for;

   fast_for i on rev(L) do 
       if not(is_prime(rev(i).packitem)) then
           return(false);
       endif;
   endfast_for;

   return(true);

enddefine;

define problem37;

    ;;; 2, 3, 5, and 7 is not considered truncable primes
    ;;; so we start on 9
    lvars p = 7+2; 
    lvars sum = 0;
    lvars c = 0;
    while c < 11 do
        if is_prime(p) then
            if check2(p) then
                c + 1->c;
                ;;; [p ^p]=>
                sum+p->sum;
            endif;
        endif;

        p+2->p;
    endwhile;

    sum=>

enddefine;

'problem37()'=>
problem37();
timediff();


