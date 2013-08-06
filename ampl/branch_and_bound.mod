/*
  Wed Jan  2 10:54:24 2008/hakank@bonetmail.com

  Winston OR, page 512ff discuss branch and bound.
  (Is there a way to show the bb tree?)


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/



*/

var x1 integer >= 0;
var x2 integer >= 0;

maximize z: 8*x1 + 5*x2;

subject to 
        c1: x1 + x2 <= 6;
        c2: 9*x1 + 5*x2 <= 45;


# data;

# option cplex_options "sensitivity mipdisplay=5 bardisplay=2 conflictdisplay=2 display=2 mipinterval=3 nodefile=2 file=xxx timing=1 writeprob=xxx.lp";
# option solver cplex;
#option solver minos;
#option solver cbc;
#option solver snopt;
#option solver gjh;
#option solver ipopt;
# option solver bonmin;
option lpsolve_options "printsol=7 prlp psols psolsa trace";
option solver lpsolve;
#option solver donlp2;
#option solver loqo;

solve;

display z;
display x1,x2;

end;
