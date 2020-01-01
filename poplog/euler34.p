/*

  Euler problem 34
  """
  145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
  
  Find the sum of all numbers which are equal to the sum of the 
  factorial of their digits.
  
  Note: as 1! = 1 and 2! = 2 are not sums they are not included.
  """ 

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/

compile('/home/hakank/Poplib/init.p');

define fact(n);
    if n= 0 then 1 else n*fact(n-1); endif;
enddefine;

;;; Don't do much difference here...
newmemo(fact, 10)->fact;

define problem34;
    lvars sum = 0;
    lvars i;
    for i from 3 to 50000 do 
        lvars s = 0, j;
        for j in i.unpackitem do
            s+fact(j)->s;
        endfor;
        if s = i then
            sum+i->sum;
        endif
    endfor;

    sum=>;

enddefine;


'problem34()'=>
problem34();


