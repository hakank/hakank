/*
   Tue Nov 25 22:51:04 2008/hakank@bonetmail.com

*/
compile('/home/hakank/Poplib/init.p');


;;; From http://www.cs.bham.ac.uk/research/projects/poplog/primer/node155.html
define gen_evens() -> next;
    ;;; create a local, constant, private list containing 0
    ;;; the number will be changed each time the procedure is run.
    lconstant store = [0];
    
    store(1) -> next;
    next + 2 -> store(1);
enddefine;


define gen_fib() -> next;
    ;;; create a local, constant, private list containing 0
    ;;; the number will be changed each time the procedure is run.
    lconstant store = [1 1];
   
    lvars tmp = store(1);
    store(2) -> store(1);
    store(2) -> next;
    tmp + store(2) -> store(2);
enddefine;



;;; gen_evens() =>
;;; gen_evens() =>
;;; gen_evens(), gen_evens(), gen_evens() =>

;;; convert to dynamic list
vars gen_list = pdtolist(gen_evens);
gen_list =>
gen_list(1)=>
gen_list(2) =>
gen_list =>
gen_list(15) =>
gen_list =>

vars fib_list = pdtolist(gen_fib);
/*
fib_list=>
fib_list(10)=>
fib_list=>
fib_list(100)=>
fib_list=>
*/

lvars i = 1;
lvars sum = 0;
[%while fib_list(i) < 4000000 do
      if (fib_list(i)) mod 2 = 0 then
          fib_list(i) + sum -> sum;
      endif;
      i + 1 -> i
endwhile%]=>;

sum=>
'fib_list'=>
fib_list=>

;;; brutal version. TODO: get something better...
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
        i+1-> i;
    enduntil;
    i -> store(1);
enddefine;

vars prime_list = pdtolist(gen_prime);

prime_list(100)=>
prime_list=>

prime_list(1000)=>
