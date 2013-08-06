/*

  Euler problem 26
  """
  A unit fraction contains 1 in the numerator. The decimal representation of the 
  unit fractions with denominators 2 to 10 are given:
 
      1/2	= 	0.5
      1/3	= 	0.(3)
      1/4	= 	0.25
      1/5	= 	0.2
      1/6	= 	0.1(6)
      1/7	= 	0.(142857)
      1/8	= 	0.125
      1/9	= 	0.(1)
      1/10	= 	0.1
 
  Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be 
  seen that 1/7 has a 6-digit recurring cycle.
 
  Find the value of d < 1000 for which 1/d contains the longest recurring cycle in 
  its decimal fraction part.
  """ 

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/

compile('/home/hakank/Poplib/init.p');

;;; http://www.perlmonks.org/?node_id=637880
;;; Regexp /\.\d*?(\d+?)\1/;

;;;
;;; Get the length of the repeating cycle for 1/n
;;;
define get_rep_len(i);
    lvars foundRemainders = initintvec(i+1);
    lvars value = 1;
    lvars position = 1;
    while foundRemainders(value+1) = 0 and value /= 0 do
        position->foundRemainders(value+1);
        value*10->value;
        value mod i->value;
        position+1->position;
    endwhile;

    return(position-foundRemainders(value+1));
enddefine;

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


define problem26;

    lvars d;
    lvars max_len = 0;
    lvars max_d = 0;
    for d from 2 to 999 do
        ;;; Checking primes is faster for larger limits
        if is_prime(d) then
            lvars len=get_rep_len(d);
            if len > max_len then
                len->max_len;
                d->max_d;
            endif;
        endif;
    endfor;

    max_d=>;

enddefine;

'problem26()'=>
problem26();


