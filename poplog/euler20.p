/*

  Euler problem 20
  """
  n! means n (n 1) ... 3 2 1

  Find the sum of the digits in the number 100!

  """ 


  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/

define factorial(n)->fact;
    lvars fact=1;
    lvars i;
    for i from 1 to 100 do 
        fact*i->fact; 
    endfor; 
enddefine;

;;; Variant using applist instead
;;; (this is the one used below).
define factorial2(n)->fact;
    lvars i;
    lvars fact=applist(1, [%for i from 1 to n do i; endfor%], nonop *);
enddefine;


define problem20();
    ;;; lvars sum=0;
    ;;; factorial(100).unpackitem.applist(%procedure(e); sum+e->sum; endprocedure%);
    ;;; sum=>;

    ;;; Shorter alternative using applist
    applist(0, factorial2(100).unpackitem, nonop +)=>;
enddefine;


'problem20()'=>
problem20();
