/*

  Euler problem 31
  """
  In England the currency is made up of pound, £, and pence, p, and there are 
  eight coins in general circulation:
 
     1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
 
  It is possible to make £2 in the following way:
 
     1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
 
  How many different ways can £2 be made using any number of coins?
  """ 

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/

compile('/home/hakank/Poplib/init.p');

vars coins=[1 2 5 10 20 50 100 200];

define f(amount, num_coins);
    if num_coins == 1 then
        return(1);
    elseif amount >= 0 then
        return(f(amount-coins(num_coins), num_coins) + 
               f(amount, num_coins-1));
    else
        return(0);
    endif;

enddefine;

define problem31;
    f(200, length(coins))=>;
enddefine;


'problem31()'=>
problem31();


