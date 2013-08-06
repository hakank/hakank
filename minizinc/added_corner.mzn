%
% Added corner puzzle in Minizinc
%
% Problem from http://www.delphiforfun.org/Programs/AddedCorners.htm
% """
% This puzzle requires that you enter the digits 1 through 8 in the circles and squares (one digit in each figure) so that the number in each square is equal to the sum on the numbers in the circles which  adjoin it.  
% ...
% 
%    C F C
%    F   F
%    C F C
% """
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc


include "globals.mzn";

var 1..8: A;
var 1..8: B;
var 1..8: C;
var 1..8: D;
var 1..8: E;
var 1..8: F;
var 1..8: G;
var 1..8: H;


solve satisfy;

constraint
  all_different([A,B,C,D,E,F,G,H]) /\
  B = A + C  /\
  D = A + F  /\
  E = C + H  /\
  G = F + H
;

output [
  show(A), " ", show(B), " ", show(C), "\n", 
  show(D), "   ",  show(E), "\n", 
  show(F), " ", show(G), " ", show(H),"\n"
];

