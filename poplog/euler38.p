/*

  Euler problem 38
  """
  Take the number 192 and multiply it by each of 1, 2, and 3:
 
      192 × 1 = 192
      192 × 2 = 384
      192 × 3 = 576
 
  By concatenating each product we get the 1 to 9 pandigital, 
  192384576. We will call 192384576 the concatenated product of 192 
  and (1,2,3)
 
  The same can be achieved by starting with 9 and multiplying by 
  1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the 
  concatenated product of 9 and (1,2,3,4,5).
 
  What is the largest 1 to 9 pandigital 9-digit number that can be 
  formed as the concatenated product of an integer with 
  (1,2, ... , n) where n > 1?
  """ 

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/

compile('/home/hakank/Poplib/init.p');

define is_pandigital(ll);
    if length(ll) = 9 and not(issubstring('0',ll)) then
        lvars hash=newmapping([], 9, 0, true);
        lvars l;
        for l in ll.unpackitem do 1->hash(l); endfor;
        if [%explode(hash)%].length = 9 then
            return(true);
        endif;
    endif;
    return(false);
enddefine;


define problem38;

    lvars max_n = 0;
    lvars s = '';
    lvars n;
    for n from 9876 by - 1 to 9 do
        n >< ''->s;
        if s(1) = 49+9 then
            nextloop;
        endif;
        lvars i = 2;
        while length(s) < 9 do 
            s >< (n*i) -> s;
            i+1->i;
        endwhile;
        lvars len = length(s);
        if i = 2 or strnumber(s) < 918273645 or len /= 9 then
            nextloop;
        endif;
        if len = 9 and is_pandigital(s) then
            s->max_n;
            quitloop;
        endif;

    endfor;

    max_n=>
enddefine;

'problem38()'=>
problem38();


