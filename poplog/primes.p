/*

   Simple prime generation program in Pop-11.

   This program use dynamic lists for generating primes,
   where the definition of primes and the generator are
   quite simple.

   This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com
   See also my Pop-11/Poplog page: http://www.hakank.org/poplog/


*/

;;;
;;; Define a prime (simple version)
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
;;; define the generation of primes
;;;
define gen_prime()->next;
    lconstant store = [2]; ;;; initialise with the first prime
    
    store(1) -> next;
    lvars i = store(1)+1;
    until is_prime(i) do
        i + 1 -> i;
    enduntil;
    i -> store(1);
enddefine;

;;; convert to a dynamic list
vars prime_list = pdtolist(gen_prime);

;;; the 100'th prime
prime_list(100)=>
;;; the list so far (up to the 100'th prime)
prime_list=>

;;; print the 10000'th prime
prime_list(10000)=>
