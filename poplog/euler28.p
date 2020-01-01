/*

  Euler problem 28
  """
  Starting with the number 1 and moving to the right in a clockwise direction a 
  5 by 5 spiral is formed as follows:
 
  21 22 23 24 25
  20  7  8  9 10
  19  6  1  2 11
  18  5  4  3 12
  17 16 15 14 13
 
  It can be verified that the sum of the numbers on the diagonals is 101.
 
  What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral 
  formed in the same way?
  """ 

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/

compile('/home/hakank/Poplib/init.p');

define problem28;

    lvars s=0;
    lvars n;
    for n from 2 by 2 to 1001 do
        s+4*(n-1)**2+10*(n-1)->s;
    endfor;

    s+1=>

enddefine;


'problem28()'=>
problem28();


