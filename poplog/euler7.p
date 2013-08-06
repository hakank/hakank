/*

  Problem 7
  """
  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can 
  see that the 6th prime is 13, we can see that the 6th prime is 13.

  What is the 10001st prime number?

  """

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/


*/
compile('/home/hakank/Poplib/init.p');

;;;
;;; brutal version.
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

define gen_prime()->next;
    lconstant store = [2];
    
    store(1) -> next;
    lvars i = store(1)+1;
    until is_prime(i) do
        i + 1 -> i;
    enduntil;
    i -> store(1);
enddefine;

define problem7();
   vars prime_list = pdtolist(gen_prime);
   prime_list(10001)=>
enddefine;


'problem7()'=>
problem7();
