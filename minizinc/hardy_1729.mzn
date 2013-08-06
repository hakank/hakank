% 
% Hardy's 1729 problem in MiniZinc.
% 
% From Hardy "A Mathematician's Apology"
% """
% I had ridden in taxi cab 1729 and remarked that the number seemed to
% be rather a dull one. I hoped it was not an unfavorable omen.
% "No", he replied, "it is a very integresting number: it is the smallest
% number expressed as the sum of two cubes in two different ways.
% """
% 

% Solution:
%  X1:  1
%  X2: 12
%  Y1:  9
%  Y2: 10
%   Z: 1729 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

var int: X1;
var int: X2;
var int: Y1;
var int: Y2;
var int: Z;


solve satisfy;
% solve minimize Z;

constraint
  X1 > 0 /\ X2 > 0 /\  Y1 > 0 /\ Y2 > 0 /\ Z > 0
  /\
  X1 <= X2 /\ Y1 <= Y2  % symmetry breaking
  /\
  X1 != Y1 /\ X1 != Y2
  /\
  X1*X1*X1 + X2*X2*X2 = Z
  /\
  Y1*Y1*Y1 + Y2*Y2*Y2 = Z
;


output [
  "X1: ", show(X1), "\n",
  "X2: ", show(X2), "\n",
  "Y1: ", show(Y1), "\n",
  "Y2: ", show(Y2), "\n",
  "Z : ", show(Z), "\n",

];