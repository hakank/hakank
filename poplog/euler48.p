/*

  Euler problem 48
  """
  The series, 1^(1) + 2^(2) + 3^(3) + ... + 10^(10) = 10405071317.
   
  Find the last ten digits of the series, 
  1^(1) + 2^(2) + 3^(3) + ... + 1000^(1000).
  """ 

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/

compile('/home/hakank/Poplib/init.p');

define problem48;
    lvars n = 1000,
         sum = 0, 
         i;
    
    for i to n do 
        sum+i**i->sum
    endfor;

    lvars ssum = sum><'';
    substring(ssum.length-9, 10, ssum)=>
         

enddefine;


;;; More compact.
define problem48b;
    lvars n = 1000;
    lvars i;
    lvars ssum = applist(0,[%for i to n do i**i endfor%], nonop +)><'';
    substring(ssum.length-9, 10, ssum)=>

enddefine;



;;; 'problem48()'=>
;;; problem48();

'problem48b()'=>
problem48b();
