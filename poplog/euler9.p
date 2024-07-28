/*

  Problem 9
  """
  A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,
  a^2 + b^2 = c^2

  For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  Find the product abc.
  """

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/


*/
compile('/home/hakank/Poplib/init.p');


define is_pyth(a,b,c);

  a**2+b**2=c**2

enddefine;


define problem9();
    ;;; brute force
    lvars a,b,c;
    for c from 1 to 500 do 
        for b from 1 to c do 
            for a from 1 to b do 
                if a+b+c=1000 and is_pyth(a,b,c) then 
                    ;;; [^(a * b * c) ^a ^b ^c]
                    a * b * c
                endif 
            endfor 
        endfor 
    endfor=>

enddefine;


'problem9()'=>
problem9();
timediff();