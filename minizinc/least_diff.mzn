%
% Least diff problem in MiniZinc.
% 
% The model solves the following problem:
% 
% What is the smallest difference between two numbers X - Y
% if you must use all the digits (0..9) exactly once, i.e.
% Minimize the difference 
%   ABCDE - FGHIJ
%
% Cf http://www.hakank.org/constraints/LeastDiff2.java
%
%
%
% This Minizinc model was written by Hakan Kjellerstrand and is commented in 
% (the Swedish) blogpost
% "Constraint Programming: Minizinc, Gecode/flatzinc och ECLiPSe/minizinc"
% http://www.hakank.org/webblogg/archives/001209.html
%

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

% use letters for a readable representation 
int: n = 10;
int: m = n div 2;
array[1..m] of int: bases = [ceil(pow(10.0,int2float(m-i))) | i in 1..m];

% decision variables
set of int: digits = 0..9; % declare the range 
var digits: A;
var digits: B;
var digits: C;
var digits: D;
var digits: E;

var digits: F;
var digits: G;
var digits: H;
var digits: I;
var digits: J;

% and using an array for the all_different predicate
array[1..n] of var digits: FD =  [A, B, C, D, E, F, G, H, I, J];

var int: X;
var int: Y;
var int: difference = X - Y;


%
% scalar_product(a1, a2, result)
% - a1, a2: two arrays of equal lengths
% - result: the result of summing the product of a1[i]*a2[i]
% 
predicate scalar_product(array[int] of var int: arr1, array[int] of var int: arr2, var int: result) =
          assert(card(index_set(arr1)) == card(index_set(arr2)), 
            "scalar_product: arr1 and arr2 must be of same length.",
             result = sum(i in index_set(arr1)) (
                arr1[i] * arr2[i]
             )
          )
;

constraint
      % more general model
      scalar_product(bases, [A,B,C,D,E], X) /\
      scalar_product(bases, [F,G,H,I,J], Y) /\

      % Instead of using scalar_product, it could be modelled instead as:
      % X = (10000*A +1000*B +100*C +10*D + E) /\
      % Y = (10000*F +1000*G +100*H +10*I + J) /\

      % all number must be equal
      all_different(FD) /\

      % the difference must be positive
      % difference = X - Y /\
      difference > 0
; 



% solve minimize difference;

% this solve hint makes it somewhat faster
solve :: int_search(FD, max_regret, indomain_min, complete) minimize difference;
      

% result
output [
  show(FD) ++ "\n" ++
  show(X) ++ " - " ++ show(Y) ++ " = " ++ show(difference) ++ "\n"
];
