% 
% Nine-digit arrangement in MiniZinc.
% 
% Martin Garder. April 1972.
% 
% Arrange the digits 1..9 in two multiplications which give the 
% same product,
%    ABC    FG
%  *  DE  * HI
%  -----  ----
%  ????   ????

% 
% There are 11 solutions to this equation.
% The maximum product is 7448 which has the following solution:
%
%   532
% *  14
% -----
%  7448
%
%    76
%  * 98
% -----
%  7448
%
% 


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

set of 1..9: d = 1..9;
var d: A;
var d: B;
var d: C;
var d: D;
var d: E;
var d: F;
var d: G;
var d: H;
var d: I;

var int: s; % the sum

array[d] of var d: x = [A,B,C,D,E,F,G,H,I];


% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) maximize s;

constraint

   all_different(x)

   /\
   s = (100*A + 10*B + C) * (10*D + E) 
   /\
   s = (10*F + G) * (10*H + I) 
   /\
   (10*F + G) <= (10*H + I)  % symmetry breaking
   /\
   s = 7448
;


output [
  "Solution:\n",
  "  ", show(A), show(B), show(C), "\n",
  "*  ", show(D), show(E), "\n",
  "-----\n",
  " ", show(s), "\n",
  "\n", 
  "   ", show(F), show(G), "\n",
  " * ", show(H), show(I), "\n",
  "-----\n",
  " ", show(s),"\n",

];
