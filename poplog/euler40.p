/*

  Euler problem 40
  """
  An irrational decimal fraction is created by concatenating the positive integers:
  
  0.123456789101112131415161718192021...
  
  It can be seen that the 12th digit of the fractional part is 1.
  
  If dn represents the nth digit of the fractional part, find the 
  value of the following expression.
 
  d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
  """ 

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/

compile('/home/hakank/Poplib/init.p');

define problem40;

    lvars i;
    ;;; This is very slow
    ;;; for i from 1 to 1000000 do
    ;;;     d><i->d;
    ;;; endfor;

    ;;; Much faster
    lvars d = [% fast_for i from 1 to 1000000 do 
                          i><''
                      endfast_for %].packitem><'';

    lvars prod = 1;
    for i from 1 to 6 do
        prod*(d(10**i)-48)->prod;
    endfor;

    prod=>

enddefine;

'problem40()'=>
problem40();


