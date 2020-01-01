/*

  Euler problem 32
  """
  We shall say that an n-digit number is pandigital if it makes use of all 
  the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 
  1 through 5 pandigital.
  
  The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing 
  multiplicand, multiplier, and product is 1 through 9 pandigital.
  
  Find the sum of all products whose multiplicand/multiplier/product identity 
  can be written as a 1 through 9 pandigital.
  HINT: Some products can be obtained in more than one way so be sure 
  to only include it once in your sum.
  """ 

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/

compile('/home/hakank/Poplib/init.p');


define problem32;
    lvars a,b;
    lvars sum = 0;
    lvars prodhash = newmapping([], 20, 0, true);
    for a from 2 to 98 do
        for b from a+1 to 9876 do
            lvars prod = a*b;
            lvars ll = a >< b >< prod; ;;; make a string 
            if length(ll) = 9 and not(issubstring('0',ll)) then
                lvars hash=newmapping([], 9, 0, true);
                lvars l;
                for l in ll.unpackitem do 1->hash(l); endfor;
                if [%explode(hash)%].length = 9 and prodhash(prod)=0 then
                    ;;; [^a * ^b = ^prod => ^ll]=>;
                    sum+prod->sum;
                    1->prodhash(prod);
                endif;
            endif;
        endfor;
    endfor;

    sum=>;

enddefine;


'problem32()'=>
problem32();


