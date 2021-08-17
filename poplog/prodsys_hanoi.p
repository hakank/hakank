/*
  
   Test of Prodsys in Pop-11

   Towers of Hanoi using Prodsys.

   See HELP PRODYS.


   This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com
   See also my Pop-11/Poplog page: http://www.hakank.org/poplog/


*/
compile('/home/hakank/Poplib/init.p');

lib prodsys;

nil -> rulebase;

true -> repeating;
false -> chatty;
false -> walk;  ;;; This is kind of tracing. Press Enter to fire the rules.
true -> backtracking;

/*

;;; From prodsys_test2.p
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
*/

/*
define hanoi(n, src, dst, via);
if n > 0 then
    hanoi(n - 1, src, via, dst);
    'Move disk ' >< n >< ' from ' >< src >< ' to ' >< dst >< '.' =>
    hanoi(n - 1, via, dst, src);
endif;
enddefine;

hanoi(3, "left", "middle", "right");

bye;

** Move disk 1 from left to middle.
** Move disk 2 from left to right.
** Move disk 1 from middle to right.
** Move disk 3 from left to middle.
** Move disk 1 from right to left.
** Move disk 2 from right to middle.
** Move disk 1 from left to middle.

*/

rule 1 [hanoi 1 ?src ?dst ?via] ;
   [move disk 1 from ^src to ^dst]=>;
   remove([hanoi 1 ?src ?dst ?via]);
   add([move 1 from ^src to ^dst]);
endrule;

rule 2 [hanoi ?n ?src ?dst ?via];
   remove([hanoi ^n ^src ^dst ^via]);
   if n > 0 then
       add([hanoi ^(n-1) ^src ^via ^dst]);
       add([move ^(n-1) from ^src to ^dst]);
       [move disk ^n from ^src to ^dst]=>;
       add([hanoi ^(n-1) ^via ^dst ^src]);
       add([move ^(n-1) from ^via to ^dst]);
   endif;

endrule;

[ [hanoi 3 left middle right]] -> database;

run();

;;; This shows the result
database=>;
