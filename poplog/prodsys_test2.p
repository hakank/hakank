/*
  
   Test of Prodsys in Pop-11

   See HELP PRODYS.


   This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com
   See also my Pop-11/Poplog page: http://www.hakank.org/poplog/


*/
compile('/home/hakank/Poplib/init.p');

lib prodsys;

nil -> rulebase;

false -> repeating;
true -> chatty;
false -> walk;  ;;; This is kind of tracing. Press Enter to fire the rules.
false -> backtracking;

;;;
;;; This counts the factorial of n
;;;
rule 1 [part_result fact ?n 1] ;
   remove([part_result fact ^n 1]);
   add([result ^n]);
endrule;

rule 2 [part_result fact ?n ?m] ;
   remove([part_result fact ^n ^m]);
   add([part_result fact ^(n*m) ^(m-1)]);
endrule;

[[part_result fact 1 7]] -> database;

run();

;;; This shows the result
database=>;



