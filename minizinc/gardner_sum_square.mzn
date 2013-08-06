% 
% Gardner's Sum Square problem in MiniZinc.
% 
% From Scampi's model GardnerSumSquare.scala
% """
% Martin Garner Problem:
% Let a,b,c,d,e,f,g and h be distinct elements in the set {-7,-5,-3,-2,2,4,6,13}
% What is the minimum possible value of (a+b+c+d)^2 + (e+f+g+h)^2
% @author Pierre Schaus pschaus@gmail.com
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
set of int: S = {-7,-5,-3,-2,2,4,6,13};

var S: a;
var S: b;
var S: c;
var S: d;
var S: e;
var S: f;
var S: g;
var S: h;

array[1..8] of var S: x = [a,b,c,d,e,f,g,h];

var int: V;

% solve satisfy;
% solve minimize V;
solve :: int_search(x, 
        max_regret, 
        indomain_min, 
        complete) minimize V;

constraint
  alldifferent(x) 
  /\
  V = (a+b+c+d)*(a+b+c+d) + (e+f+g+h)*(e+f+g+h)

  /\ % symmetry breaking
  increasing([a,b,c,d])
  /\
  increasing([e,f,g,h])
  /\
  a < e
;

output [
  "x: " ++ show(x) ++ "\n" ++
  "V: " ++ show(V)
]
 ++ ["\n"]
;

