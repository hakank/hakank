%
% Puzzle in Minizinc.
% 
% http://www.delphiforfun.org/Programs/brute_force.htm
%
% "What 3 digit number together with it's square contains all 10 digits?"
%
% Solutions (allows A and D to be 0)
%  N = 854, N^2 = (0)729316
%  N = 567  N^2 = (0)321489
% 
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

var 0..9: A;
var 0..9: B;
var 0..9: C;
var 0..9: D;
var 0..9: E;
var 0..9: F;
var 0..9: G;
var 0..9: H;
var 0..9: I;
var 0..9: J;

var int: N;

solve satisfy;

constraint
   all_different([A,B,C,D,E,F,G,H,I,J]) /\
   N = 100*A + 10*B + C /\  % the 3 digit number
   N*N = 1000000*D + 100000*E +  10000*F + 1000*G + 100*H + 10*I + J % the square
   % /\ A > 0 /\ D > 0
;


output [show([A,B,C,D,E,F,G,H,I,J])];