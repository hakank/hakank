/*

  Problem 16
  """
  2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

  What is the sum of the digits of the number 2^1000?
  """

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/
compile('/home/hakank/Poplib/init.p');

define sumlist(list) -> res;
   applist(0, list, nonop + ) -> res;
enddefine;


define problem16();
    (2**1000).unpackitem.sumlist=>
enddefine;


;;; Just with basic operations
define problem16b();
    lvars s, i;
    0->s; for i in ((2**1000).unpackitem) do i+s -> s; endfor; 
    s=>

enddefine;


'problem16()'=>
problem16();

;;; 'problem16b()'=>
;;; problem16b();
