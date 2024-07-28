/*

  Problem 6
  """
  The sum of the squares of the first ten natural numbers is,
  1^2+ 22 ... + 10^2 = 385

  The square of the sum of the first ten natural numbers is,
  (1 + 2 + ... + 10)^2= 55^2 = 3025

  Hence the difference between the sum of the squares of the first ten 
  natural numbers and the square of the sum is 3025 385 = 2640.

  Find the difference between the sum of the squares of the first one 
  hundred natural numbers and the square of the sum.
  """

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/


*/
compile('/home/hakank/Poplib/init.p');

define problem6();
   lvars n = 100;
   lvars i;
   lvars list= [%for i from 1 to n do i endfor%];
   lvars sum_squares, squares_sum, sdiff;
   applist(0, [%applist(list, procedure(n); n*n endprocedure)%], nonop +) -> sum_squares;
   applist(0, list, nonop +)**2 -> squares_sum;
   (squares_sum - sum_squares) -> sdiff;
   ;;; [^squares_sum - ^sum_squares = ^sdiff]=>;
   sdiff;
enddefine;

'problem6()'=>
problem6()=>;
timediff()=>;
