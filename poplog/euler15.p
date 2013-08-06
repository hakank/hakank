/*

  Problem 15
  """
  Starting in the top left corner of a 2×2 grid, there are 6 routes 
  (without backtracking) to the bottom right corner.

  How many routes are there through a 20×20 grid?
  """

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/
compile('/home/hakank/Poplib/init.p');

define productlist(list) -> res;
   applist(1, list, nonop * ) -> res;
enddefine;

;;; range(from, to) -> list
define range(x_low, x_up);
    lvars i;
    [%for i from x_low to x_up do i endfor%];
enddefine;

define problem15();
  productlist(range(21,40)) / productlist(range(2,20))=>
enddefine;


'problem15()'=>
problem15();