/*

  Euler problem 46
  """
  It was proposed by Christian Goldbach that every odd composite number can be 
  written as the sum of a prime and twice a square.
  
  9 = 7 + 2×1^2
  15 = 7 + 2×2^2
  21 = 3 + 2×3^2
  25 = 7 + 2×3^2
  27 = 19 + 2×2^2
  33 = 31 + 2×1^2
  
  It turns out that the conjecture was false.
  
  What is the smallest odd composite that cannot be written as the 
  sum of a prime and twice a square?
  """ 

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/

compile('/home/hakank/Poplib/init.p');

define is_prime(n);
    lvars i;
    if n = 2 or n = 3 then
        return(true);
    endif;
    if n mod 2 = 0 then
        return(false);
    endif;
    for i from 3 by 2 to round(sqrt(n))+1 do
        if n mod i = 0 then
            return(false);
        endif;
    endfor;
    return(true);
enddefine;


define problem46;

    lvars i;
    for i from 3 by 2 to 10000 do 
        if is_prime(i) then nextloop; endif;
        lvars s = sqrt(i/2);
        lvars found = false;
        lvars j;
        for j from 1 to s do
            lvars ts = j*j*2;
            if is_prime(i-ts) then
                true->found;
                nextloop;
            endif;
        endfor;
        
        if not(found) then
            quitloop;
        endif;
    endfor;

    i=>

enddefine;


'problem46()'=>
problem46();


