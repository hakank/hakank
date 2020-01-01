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
compile('/home/hakank/Poplib/init.p');


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
;;; Kind of brutal brutal...
;;;
define factors(n);
    lvars m = n;
    lvars ll = [];

    ;;; if is_prime(n) then 
    ;;;    return([]);
    ;;; endif;
    lvars i;
    [% 
      for i from 2 to round(m/2) do
         if m mod i = 0 then
             while m mod i = 0 then
                 i;
                 m / i -> m;
             endwhile;
         endif;
         if m = 1 then quitloop endif;
     endfor;
     %]->ll;
    return(ll);
enddefine;




;;;
;;; Simpler version: loop until the last prime is detected
;;;
define problem3();

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

;;; 'problem3()'=>
;;; problem3();

'problem3b()'=>
problem3b()=>;
