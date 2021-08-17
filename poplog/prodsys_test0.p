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
false -> walk; 
false -> backtracking;

rule 1 [wear sunglasses] [forecast rain] ;
  remove([wear sunglasses]);
  add([wear raincoat]);
endrule;

rule 2 [wear ?x] [forecast rain] ;
   remove([wear ^x]);
   add([wear raincoat]);
endrule;

[[wear sunglasses] [forecast rain]] -> database;
run();

;;; This shows the result
database=>;



