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


define problem25;
    ;;; Memoize the function
    newmemo(fib, 20) -> fib;

    lvars len = 0;
    lvars f = 0;
    while fib(f).unpackitem.length < 1000 do        
        f+1->f;
    endwhile;

    f=>;

enddefine;

'problem25()'=>
problem25();


