% 
% Numerica problem page 19 in MiniZinc.
% 
% Problem from the Numerica book, page 20
% 
% Finding intersection of a circle and a parabola:
%   x^2+y^2=1
%   x^2=y
%
% Gecode give four solutions which is a duplicate of the
% two correct solutions:
%
%  x:0.786151377757
%  y:0.618033988750
%  ----------
%  x:-0.786151377757
%  y:0.618033988750
%  ----------
%  ==========
%
% JaCoP give just these two solutions:
%   x:-0.786151377757
%   y:0.618033988750
%   ----------
%   x:0.786151377757
%   y:0.618033988750
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

float: v = pow(10.0,8.0);

var -v..v: x;
var -v..v: y;


% solve satisfy;
solve :: float_search([x,y], 0.00000000000001, input_order, indomain_split, complete) satisfy;

constraint
   x*x + y*y = 1.0 
   /\
   x*x = y
   
;

output [
  "x:" ++ show_float(3,12,x) ++ "\n" ++
  "y:" ++ show_float(3,12,y)
];

