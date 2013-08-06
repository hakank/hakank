%
% Send more money problem. in Minizinc.
%
% This Minizinc program was written by Hakan Kjellerstrand and is commented in 
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
var 0..9: S;
var 0..9: E;
var 0..9: N;
var 0..9: D;
var 0..9: M;
var 0..9: O;
var 0..9: R;
var 0..9: Y;

array[1..8] of var int : fd =
  [S,E,N,D,M,O,R,Y];


constraint
  all_different(fd) /\
            1000*S + 100*E + 10*N + D  +  
            1000*M + 100*O + 10*R + E  = 
  10000*M + 1000*O + 100*N + 10*E + Y 
  /\
  S > 0 /\
  M > 0
;  


solve satisfy;

output [
%   show(fd), "\n",
   "S:", show(S), " E:", show(E), " N:", show(N), " D:", show(D), 
   " M:", show(M), " O:", show(O), " R:", show(R), " Y:", show(Y),
   "\n\n",

   "    ", show(S), show(E), show(N), show(D),"\n",
   " +  ", show(M), show(O), show(R), show(E),"\n",
   " = ",  show(M), show(O), show(N), show(E), show(Y), "\n"

];
