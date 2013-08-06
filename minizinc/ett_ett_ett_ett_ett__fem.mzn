%
% Swedish puzzle in Minicinc
%
% ETT+ETT+ETT+ETT+ETT=FEM
%
% (translator note: "ett" is one, and "fem" is five)
% 
% [This was a show case for solving puzzles in Excel/Openoffice Calc.]
% 
%
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn";
var 0..9: E;
var 0..9: T;
var 0..9: F;
var 0..9: M;

array[1..4] of var int : FD =
  [E,T,F,M];

constraint
  100*E + 10*T + T  +  
  100*E + 10*T + T  +  
  100*E + 10*T + T  +  
  100*E + 10*T + T  +  
  100*E + 10*T + T  =
  100*F + 10*E + M  
  /\ 
  E > 0 
  /\ 
  F > 0
  /\
  all_different([E,T,F,M])
;  



solve satisfy;

output [
   show(E),show(E),show(T), "+",
   show(E),show(E),show(T), "+",
   show(E),show(E),show(T), "+",
   show(E),show(E),show(T), "+",
   show(E),show(E),show(T), " = ",
   show(F),show(E),show(M),"\n",
];
