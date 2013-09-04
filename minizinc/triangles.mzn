% 
% Triangle problems in MiniZinc.
% 
% From "Fun with Num3ers": 
% "Digits from 0 to 9 used only once"
% http://benvitalenum3ers.wordpress.com/2013/08/16/digits-from-0-to-9-used-only-once/
% """
%               A
%            B    C
%         D    E    F
%      G    H     I   J
%
%
% Replace the letters A, B, C, D, E, F, G, H, I, J with 
% with a different number from 0 to 9, such that
% 
%   A + C + F + J   =   A + B + D + G   =   G + H + I + J
% and
%   A + B + C   =   D + G + H   =   F + I + J
% """

% There are 12 solutions:
%
%          8
%        2  3
%      0  6  7
%    9  4   5  1
% ----------
%
%          9
%        0  4
%      2  6  5
%    8  3   7  1
% ----------
%
%          1
%        6  7
%      2  3  9
%    8  4   5  0
% ----------
%
%          1
%        7  5
%      3  6  4
%    8  2   0  9
% ----------
%
%          0
%        5  9
%      4  3  7
%    8  2   6  1
% ----------
%
%          1
%        5  7
%      4  6  3
%    9  0   2  8
% ----------
%
%          9
%        4  0
%      5  6  2
%    1  7   3  8
% ----------
%
%          8
%        4  2
%      5  3  6
%    0  9   7  1
% ----------
%
%          8
%        2  4
%      6  3  5
%    1  7   9  0
% ----------
%
%          8
%        3  2
%      7  6  0
%    1  5   4  9
% ----------
%
%          0
%        9  5
%      7  3  4
%    1  6   2  8
% ----------
%
%          1
%        7  6
%      9  3  2
%    0  5   4  8
%
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
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

array[1..10] of var 0..9: X = [A,B,C,D,E,F,G,H,I,J];


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  all_different(X) /\
  A + C + F + J  =  A + B + D + G /\
  A + B + D + G  =   G + H + I + J /\
  A + B + C   =   D + G + H /\ 
  D + G + H =   F + I + J

;

output [
  show(X), "\n",
  "         ", show(A), "\n",
  "       ",  show(B), "  ", show(C), "\n",
  "     ", show(D), "  ", show(E), "  ", show(F), "\n",
  "   ", show(G), "  ", show(H), "   ", show(I), "  ", show(J), "\n"

];

