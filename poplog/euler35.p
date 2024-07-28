/*

  Euler problem 35
  """
  The number, 197, is called a circular prime because all 
  rotations of the digits: 197, 971, and 719, are themselves prime.
  
  There are thirteen such primes below 100: 
  2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
  
  How many circular primes are there below one million?
  """ 

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
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
    fast_for i from 3 by 2 to round(sqrt(n))+1 do
        if n mod i = 0 then
            return(false);
        endif;
    endfast_for;
    return(true);
enddefine;

newmemo(is_prime, 10000)->is_prime;

define rotate(L);
    tl(L) <> [%hd(L)%]
    ;;; setfrontlist(last(L),L)
enddefine;

define problem35;
    
    ;;; we assume that we found 2, 3, and 5 already
    lvars p = 5; 
    lvars sum = 3;

    ;;; all primes must end in 1,3,7,9
    lvars ps2 = [0 2 4 5 6 8]; ;;; For early stopping
    while p < 1000000 do 

        if not(is_prime(p)) then
            p+2->p;
            nextloop;
        endif;

        ;;; skip those numbers that has bad digits
        lvars p2 = p.unpackitem;
        ;;; lvars len = length(p2);
        lvars check = true;
        lvars d;
        fast_for d in p2 do
            if fast_lmember(d, ps2) then
                false->check;
                quitloop(1);
            endif;
        endfast_for;

        if check then
            ;;; OK, now we can rotate the list
            lvars len = length(p2);
            lvars i;
            fast_for i from 1 to len-1 do
                p2.rotate->p2;
                lvars p3=p2.packitem;
                if not(is_prime(p3)) then
                    false->check;
                    quitloop(1);
                endif;
            endfast_for;
            if check then 
                sum+1->sum;
            endif;

        endif;

        p+2->p;
    endwhile;
         
    sum=>

enddefine;

'problem35()'=>
problem35();
timediff()=>;

