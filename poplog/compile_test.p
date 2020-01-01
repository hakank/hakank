/* 

   Test of compiling a Pop-11 program to 
     - a saved image 
     - an executable file

   * a) Saved image:
     Run like this to save to a saved image file (.psv):
       ;;;                 .psv file      Pop-11 file       start call
       pop11 %nort mkimage compile_test   compile_test.p    ":main();"

   And run as:
      pop11 +compile_test

   * b) Executable file
     This works on Linux.

     Run:
       ;;;     start call   Pop-11 program    exec file
       popc -e main         compile_test.p -o compile_test
   
     and then
       ./compile_test   

   This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com
   See also my Pop-11/Poplog page: http://www.hakank.org/poplog/

*/

compile_mode :pop11 +strict;

;;;
;;; Define a very simple function.
;;;
define fact(n);
  if n<=0 then 1 else n*fact(n-1)
   endif
enddefine;


define main();
  pr('hello, world\n');
  fact(100)-1==>;
  fact(1000)-1==>;
  fact(1000)/fact(999)==>;
enddefine;

;;;
;;; Start call
;;;
main();
