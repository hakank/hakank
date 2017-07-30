% 
% Chandelier Balancing (PuzzlOR) in MiniZinc.
% 
% http://puzzlor.editme.com/balance
% """
% Constructing a chandelier can be a tricky undertaking because 
% the slightest imperfection will unbalance the chandelier and cause it to be skewed.
% 
% Figure 1 shows a chandelier constructed from arms, wires, and triangles 
% that hold weights.  In order to perfectly balance the chandelier, 
% weights must be placed into the triangles.  There are 9 weights as follows: 
% 1,2,3,4,5,6,7,8,9kg.  Each triangle can only hold one weight.  
% Assume the weight of the arms, wires, and triangles are negligible.
% 
% Question:  Where should the weights be placed in order to perfectly 
% balance the chandelier?
% """
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
int: n = 9;

set of 1..n: R = 1..n;

var R: a;
var R: b;
var R: c;
var R: d;
var R: e;
var R: f;
var R: g;
var R: h;
var R: i;
var int: total = a+b+c+d+e+f+g+h+i;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  alldifferent([a,b,c,d,e,f,g,h,i])
  /\
  (2*a = 1*b+2*c) /\
  (2*d+1*e = 1*f) /\
  (2*g+1*h = 3*i) /\

  3*(a+b+c) = 2*(d+e+f)+3*(g+h+i) 

;

output [
  show(["a","b","c","d","e","f","g","h","i"]) ++ "\n" ++
  show([a,b,c,d,e,f,g,h,i])
];

