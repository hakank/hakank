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
% This is a simpler version of
%   http://www.hakank.org/minizinc/least_diff.mzn
%

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 10;

% decision variables
set of int: digits = 0..9; % declare the range 
var digits: A; var digits: B; var digits: C; var digits: D; var digits: E;
var digits: F; var digits: G; var digits: H; var digits: I; var digits: J;

array[1..n] of var digits: x =  [A, B, C, D, E, F, G, H, I, J];

var 0..99999: difference = (10000*A+1000*B+100*C+10*D+E)-
                           (10000*F+1000*G+100*H+10*I+J);

constraint all_different(x); 

% solve minimize difference;
solve :: int_search(x, max_regret, indomain_min, complete) minimize difference;

output [
  show(x) ++ "\n" ++
  show(difference) ++ "\n"
];
