/*

  Euler problem 25
  """
  The Fibonacci sequence is defined by the recurrence relation:
 
      Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
 
  Hence the first 12 terms will be:
 
      F1 = 1
      F2 = 1
      F3 = 2
      F4 = 3
      F5 = 5
      F6 = 8
      F7 = 13
      F8 = 21
      F9 = 34
      F10 = 55
      F11 = 89
      F12 = 144
 
  The 12th term, F12, is the first term to contain three digits.
 
  What is the first term in the Fibonacci sequence to contain 1000 digits?
  """ 

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/

compile('/home/hakank/Poplib/init.p');


define fib(n);
    if n <= 2 then 
        1
    else
        fib(n-1)+fib(n-2);
    endif;
enddefine;


newmemo(fib, 2000) -> fib;

;;;
;;; 0.82s
;;;
define problem25;
    ;;; Memoize the function
    ;;; newmemo(fib, 2000) -> fib;

    lvars len = 0;
    lvars f = 0;
    while fib(f).unpackitem.length < 1000 do        
        f+1->f;
    endwhile;

    f=>;

enddefine;

define fib_length(n);
    fib(n).unpackitem.length
enddefine;

;;;
;;; 0.05s
;;;
define problem25b;
    lvars target=1000;
    lvars found_upper=0;
    lvars i=1;
    lvars fib=0;
    lvars fib_len=0;
    lvars xstep=43; 

    ;;; get the upper limit
    while fib_len < target and found_upper = 0 do
        fib_length(xstep*i)->fib_len;
        if fib_len > target then
            i->found_upper;
        endif;
        i+1->i;
    endwhile;
    
    ;;; Now check all numbers from Step*(FoundUpper-1) .. Step*FoundUpper
    ;;; The target must be in that interval.
    xstep*(found_upper-1)->fib;
    fib_length(fib)->fib_len;
    while fib_len < target and fib <= xstep*found_upper do
        fib_length(fib)->fib_len;
        if fib_len < target then
            fib+1 -> fib;
        endif;
    endwhile;
 
    fib=>;

enddefine;


;;; 'problem25()'=>
;;; problem25();
;;; timediff()=>;

'problem25b()'=>
problem25b();
timediff()=>;

