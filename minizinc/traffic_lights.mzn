% 
% Traffic lights problem in MiniZinc.
% 
% CSPLib problem 16
% http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob016/index.html
% """
% Specification:
% Consider a four way traffic junction with eight traffic lights. Four of the traffic 
% lights are for the vehicles and can be represented by the variables V1 to V4 with domains 
% {r,ry,g,y} (for red, red-yellow, green and yellow). The other four traffic lights are 
% for the pedestrians and can be represented by the variables P1 to P4 with domains {r,g}.
% 
% The constraints on these variables can be modelled by quaternary constraints on 
% (Vi, Pi, Vj, Pj ) for 1<=i<=4, j=(1+i)mod 4 which allow just the tuples 
% {(r,r,g,g), (ry,r,y,r), (g,g,r,r), (y,r,ry,r)}.
%
% It would be interesting to consider other types of junction (e.g. five roads 
% intersecting) as well as modelling the evolution over time of the traffic light sequence. 
% ...
%
% Results
% Only 2^2 out of the 2^12 possible assignments are solutions.
% 
% (V1,P1,V2,P2,V3,P3,V4,P4) = 
%    {(r,r,g,g,r,r,g,g), (ry,r,y,r,ry,r,y,r), (g,g,r,r,g,g,r,r), (y,r,ry,r,y,r,ry,r)}
%    [(1,1,3,3,1,1,3,3), ( 2,1,4,1, 2,1,4,1), (3,3,1,1,3,3,1,1), (4,1, 2,1,4,1, 2,1)}
%
%
% The problem has relative few constraints, but each is very tight. Local propagation 
% appears to be rather ineffective on this problem.
%   
% """
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: n = 4;
int: r = 1; % red
int: ry = 2; % red-yellow
int: g = 3; % green
int: y = 4; % yellow

set of int: Cars = {r,ry,g,y};
set of int: Pedestrians = {r,g};

array[1..4, 1..4] of Cars: allowed;

array[1..n] of var Cars: V; % ;
array[1..n] of var Pedestrians: P; %;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

% solve satisfy;
solve :: int_search(V ++ P, first_fail, indomain_min, complete) satisfy;

constraint
 forall(i in 1..n, j in 1..n where j = (1+i) mod 4)  (
   trace("i: " ++ show(i) ++ " j: " ++ show(j) ++ "\n",1=1) /\
   exists(a in 1..4) (
      cp1d([V[i], P[i], V[j], P[j]], [allowed[a,k] | k in 1..4])
   )
 )
;


allowed = array2d(1..4, 1..4, 
[
 r,r,g,g, 
 ry,r,y,r, 
 g,g,r,r, 
 y,r,ry,r
]);


output [
  show(V[i]) ++ " " ++ show(P[i]) ++ " "
  | i in 1..n
] ++ ["\n"]
++
[
  "V: " ++ show(V) ++ "\n" ++
  "P: " ++ show(P) ++ "\n"
]
;
