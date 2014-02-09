% 
% Ages problem in MiniZinc.
% 
% Ages problem from Tony Hurlimann
% "Mathematical Modelling of Puzzle and Games"
% 
% Note: this is a MIP problem (using floats).
%

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

var 0.0..100.0: t; 
var 0.0..100.0: j; 
var 0.0..100.0: m; 
var 0.0..100.0: y; 
var 0.0..100.0: y1; 
var 0.0..100.0: y2; 
var 0.0..100.0: y3; 
var 0.0..100.0: y4; 
var 0.0..100.0: y5; 
var 0.0..100.0: y6; 
var 0.0..100.0: y7; 
var 0.0..100.0: y8;

var 0.0..100.0: Tim;
var 0.0..100.0: Jane;
var 0.0..100.0: Mary;

var 0.0..300.0: z = t+j+m;

array[1..15] of var 0.0..100.0: tmp  = [t,j,m,y,y1,y2,y3,y4,y5,y6,y7,y8,Tim,Jane,Mary];

% solve minimize z;
solve :: float_search(tmp, 0.0001, input_order,indomain_split, complete) minimize z;
% solve satisfy;

constraint
 z >= 0.0 /\

 y + 10.0 - t = 2.0*(y1 - j) /\ 
 y1 - m = 9.0*(y1 - t) /\ 
 y - 8.0 - m = 1.0 / 2.0*(y2 - j) /\ 
 y2 - j = 1.0 + y3 - t /\ 
 y3 - m = 5.0*(y + 2.0 - t) /\ 
 t + 1.0 - m = 3.0 + y4 - t /\ 
 y4 - j = 3.0*(y5 - 6.0 - m) /\ 
 y5 - j = 1.0 / 2.0*(y6 - t) /\ 
 y6 - m = 10.0 + y7 - m /\ 
 y7 - j = 1.0 / 3.0*(y8 - t) /\ 
 y8 - m = 3.0*(j - m)  /\
 
 Tim = y-t /\
 Jane = y-j /\
 Mary = y-m
;

output [
   "Tim:", show(Tim), " Jane:", show(Jane), " Mary:", show(Mary),"\n",
   show(tmp)
]




