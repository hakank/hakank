/*

  Fibonacci in Pop-11.

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com)
  See also my Pop-11/Poplog page: http://www.hakank.org/poplog/


*/
compile('/home/hakank/Poplib/init.p');

;;; No memo
define fib0(n);    
    if n <= 1 then 1
      else fib0(n-1)+fib0(n-2);
    endif
enddefine;

define fib(n);    
    if n <= 1 then 1
      else fib(n-1)+fib(n-2);
    endif
enddefine;


newmemo (fib ,20) -> fib;

fib(20)=>;
fib(100)=>;
;;; lvars i,f;
;;; for i from 1 to 1000 do
;;;     fib(i)->f;
;;;     [^i ^f]==>
;;; endfor;

trace fib0;

;;;; fib0(10)=>;
