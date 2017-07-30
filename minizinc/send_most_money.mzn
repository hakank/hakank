%
% Send most money in Minizinc.
%
% From the lecture notes:
% http://www.ict.kth.se/courses/ID2204/notes/L01.pdf
% 
% SEND + MOST + MONEY 
% and maximize MONEY
%
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn";

var 0..9: S;
var 0..9: E;
var 0..9: N;
var 0..9: D;
var 0..9: M;
var 0..9: O;
var 0..9: T;
var 0..9: Y;

var int: z = 10000*M + 1000*O + 100*N + 10*E + Y ;

array[1..8] of var int : fd =
  [S,E,N,D,M,O,T,Y];

constraint
  all_different(fd) 
  /\
            1000*S + 100*E + 10*N + D  +  
            1000*M + 100*O + 10*S + T  = 
  10000*M + 1000*O + 100*N + 10*E + Y 
  /\
  S > 0 /\
  M > 0
;  

% constraint z = 10876;


solve maximize z;
% solve satisfy;

output [
   "z: ", show(z), "\n",
   "S: ", show(S), 
   "\nE: ", show(E),
   "\nN: ", show(N),
   "\nD: ", show(D),
   "\nM: ", show(M),
   "\nO: ", show(O),
   "\nT: ", show(T),
   "\nY: ", show(Y),
   "\n"

];
