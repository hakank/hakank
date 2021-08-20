/*
  Problem 1

  http://projecteuler.net/index.php?section=problems&id=1

  """
  If we list all the natural numbers below 10 that are multiples of 3 or 5, 
  we get 3, 5, 6 and 9. The sum of these multiples is 23.

  Find the sum of all the multiples of 3 or 5 below 1000.
  """

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/
define sumlist(list) -> res;
   applist(0, list, nonop + ) -> res;
enddefine;

;;; problem1()=>
define problem1a();
  lvars ll,i;
  [%for i from 1 to 999 do if (i mod 3 = 0) or (i mod 5 = 0) then i endif endfor%] -> ll;
  applist(0, ll, nonop + )=>
enddefine;

define problem1b_tmp(n);
    lvars i, sum;
    0 -> sum;
    for i to n do 
        if i mod 3 = 0 or i mod 5 = 0 then 
            sum + i -> sum;
        endif;
    endfor;
    sum;
enddefine;

define problem1b();
   problem1b_tmp(999)=>;
enddefine;

define mod3ormod5(n);
    if n mod 3 = 0 or n mod 5 = 0 then 
        n 
    else 
        0 
    endif;
enddefine;

define problem1c;
    lvars i;
    applist(0, [%for i from 1 to 999 do mod3ormod5(i) endfor%], nonop +)=>;
enddefine;

;;; as a oneliner
define problem1d;
    lvars i;
    [%for i from 1 to 999 do if i mod 3 = 0 or i mod 5 = 0 then i endif endfor%].sumlist=>
enddefine;

'problem1a()'=>
problem1a();
timediff()=>;

;;; 'problem1b()'=>
;;; problem1b();
;;; timediff()=>;

;;; 'problem1c()'=>
;;; problem1c();
;;; timediff()=>

;;; 'problem1d()'=>
;;; problem1d();
;;; timediff()=>;
