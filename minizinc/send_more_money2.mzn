%
% Send more money problem in Minizinc.
% 
% Cf send_more_money.mzn. This version uses carries. 
%
% This Minizinc program was written by Hakan Kjellerstrand and is commented in
% Constraint Programming: Minizinc, Gecode/flatzinc och ECLiPSe/minizinc
% http://www.hakank.org/webblogg/archives/001209.html
%
%
% Send More Money
% c4 c3 c2 c1  
%    S  E  N  D
%  + M  O  R  E
%  ------------
% M  O  N  E  Y
%
% Implemented in a very straightforward way.
%
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";
var 0..9: S;
var 0..9: E;
var 0..9: N;
var 0..9: D;
var 0..9: M;
var 0..9: O;
var 0..9: R;
var 0..9: Y;

var 0..9: c1;
var 0..9: c2;
var 0..9: c3;
var 0..9: c4;

array[1..8] of var int : fd =
  [S,E,N,D,M,O,R,Y];


constraint
  all_different(fd) /\
       D + E = 10 * c1 + Y /\
  c1 + N + R = 10 * c2 + E /\
  c2 + E + O = 10 * c3 + N /\
  c3 + S + M = 10 * c4 + O /\
  c4                   = M
  
  /\
  S >= 1 /\
  M >= 1
;  


solve satisfy;

output [
   "S:", show(S), " E:", show(E), " N:", show(N), " D:", show(D),
   " M:", show(M), " O:", show(O), " R:", show(R), " Y:", show(Y),
   "\n\n",

   "    ", show(S), show(E), show(N), show(D),"\n",
   " +  ", show(M), show(O), show(R), show(E),"\n",
   " = ",  show(M), show(O), show(N), show(E), show(Y), "\n"


];
