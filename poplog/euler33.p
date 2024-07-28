/*

  Euler problem 33
  """
  The fraction 49/98 is a curious fraction, as an inexperienced mathematician in 
  attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, 
  is obtained by cancelling the 9s.
 
  We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
 
  There are exactly four non-trivial examples of this type of fraction, less than 
  one in value, and containing two digits in the numerator and denominator.
 
  If the product of these four fractions is given in its lowest common terms, find 
  the value of the denominator.
  """ 

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/

compile('/home/hakank/Poplib/init.p');


define problem33;

    lvars s = 1;
    lvars y,z;
    for y from 1 to 9 do
        for z from y to 9 do 
            lvars x = 9.0*y*z/(10*y-z);
            if floor(x)=x and 1.0*y/z < 1 and x < 10 then
                s*y/z->s;
                ;;; [^x ^y ^z ^(floor(10*y+x)) / ^(floor(z+10*x)) ^y / ^z]=>;
                ;;; printf('x:%p y:%p z:%p %p/%p %p/%p  (s:%p)\n', [^x ^y ^z ^(floor(10*y+x)) ^(floor(z+10*x)) ^y ^z ^s]);
                
            endif;
        endfor;
    endfor;

    denominator(s)=>;

enddefine;

'problem33()'=>
problem33();


