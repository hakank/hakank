/*

  Euler problem 30
  """
  Surprisingly there are only three numbers that can be written as the sum of 
  fourth powers of their digits:
 
      1634 = 1^4 + 6^4 + 3^4 + 4^4
      8208 = 8^4 + 2^4 + 0^4 + 8^4
      9474 = 9^4 + 4^4 + 7^4 + 4^4
 
  As 1 = 1^4 is not a sum it is not included.
 
  The sum of these numbers is 1634 + 8208 + 9474 = 19316.
 
  Find the sum of all the numbers that can be written as the sum of fifth 
  powers of their digits.
  """ 

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/

compile('/home/hakank/Poplib/init.p');

define check(n,p);
    lvars i, sum;
    for i in n.unpackitem do 
        sum+i**p->sum 
    endfor;
    sum = n;
enddefine;

define problem30;
    lvars p=5, sum=0, n;
    for n from 5 to 7*9**p do 
        if check(n,p) then
            sum+n->sum;
            n=>;
        endif;
    endfor;

    [result ^sum]=>;

enddefine;


'problem30()'=>
problem30();


