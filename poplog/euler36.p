/*

  Euler problem 36
  """
  The decimal number, 585 = 1001001001_(2) (binary),
  is palindromic in both bases.

  Find the sum of all numbers, less than one million, which
  are palindromic in base 10 and base 2.
 
  (Please note that the palindromic number, in either base,
  may not include leading zeros.)
  """ 

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/

compile('/home/hakank/Poplib/init.p');

define reverse(number, base)->reverse;
    lvars reverse = 0;
    while number /= 0 do
        reverse * base + number mod base->reverse;
        number div base->number;
    endwhile;
enddefine;


define palindrome(list, base);
    list = reverse(list, base);
enddefine;


define problem36;
    
    lvars sum = 0;
    lvars n;
    for n from 1 by 2 to 999999 do
        if palindrome(n,10) and palindrome(n,2) then
            sum+n->sum;
        endif;
    endfor;

    sum=>

enddefine;

'problem36()'=>
problem36();


