%
% SEND + MORE + MONEY in any base in Minizinc
%

%
% This Minizinc program is written by Hakan Kjellerstrand and is commented in
% Constraint Programming: Minizinc, Gecode/flatzinc och ECLiPSe/minizinc
% http://www.hakank.org/webblogg/archives/001209.html
%
%
% Send More Money
%    SEND
%  + MORE
%  ------
%   MONEY
%
% Implemented in a very straightforward way.
%
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

int: base = 10;
var 0..base-1: S;
var 0..base-1: E;
var 0..base-1: N;
var 0..base-1: D;
var 0..base-1: M;
var 0..base-1: O;
var 0..base-1: R;
var 0..base-1: Y;

array[1..8] of var int : fd =
  [S,E,N,D,M,O,R,Y];


constraint
  all_different(fd) /\
                          base*base*base*S + base*base*E + base*N + D  +  
                          base*base*base*M + base*base*O + base*R + E  = 
  base*base*base*base*M + base*base*base*O + base*base*N + base*E + Y 
  /\
  S >= 1 /\
  M >= 1
;  


solve satisfy;

output [
   "S:", show(S), " E:", show(E), " N:", show(N), " D:", show(D),
   " M:", show(M), " O:", show(O), " R:", show(R), " Y:", show(Y),
   "\n\n",

   "    ", show(S), " ", show(E), " ", show(N), " ", show(D),"\n",
   " +  ", show(M), " ", show(O), " ", show(R), " ", show(E),"\n",
   " = ",  show(M), " ", show(O), " ", show(N), " ", show(E), " ", show(Y), "\n"


];
