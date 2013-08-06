%
% xkcd Knapsack problem in Minizinc.
%
%  http://xkcd.com/287/
%
% Some amount (or none) of each dish should be ordered to give a total of exact 15.05.
%
% This version was inspired by Helmut Simonis presentation
%    "Acquiring Global Constraint Models"
% http://www.4c.ucc.ie/~hsimonis/freuderfest.pdf
% page 3
% 
% Which use the global constraint among_diff_0.
% ("Count how many variables are different from 0")
%
% However, this implementation differs in some ways:
%    - it use integers instead of floats.
%    - it implements a slightly more general approach
% 
% For more about the global constraint among_diff_0: see
%   http://www.hakank.org/minizinc/among_diff_0.mzn
% 
% Also see 
%   http://www.hakank.org/minizinc/xkcd.mzn
% for another approach of the problem.
% 
% (Which gives the same result as this one.)
%
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: num_prices = 6;
array[1..num_prices] of var 0..100: x; % how many items of each dish
array[1..num_prices] of int: price = [215, 275, 335, 355, 420, 580];
int: total = 1505;

var 0..num_prices: y;

solve maximize y;
% solve satisfy;

predicate among_diff_0(var int: nvar, array[int] of var int: x)=
  nvar = sum(i in index_set(x)) (
    bool2int(x[i] > 0)
  )
;


constraint
  among_diff_0(y, x)

  /\
  % Helmut's model
  % 2.15*x[1] + 2.75*x[2] + 3.35*x[3] + 3.55*x[4] + 4.20*x[5] + 5.80*x[6] =  15.05

  % Using integers instead of floats
  % 215*x[1] + 275*x[2] + 335*x[3] + 355*x[4] + 420*x[5] + 580*x[6] =  1505

  % more general approach
  total = sum(i in 1..num_prices) ( price[i]*x[i] )
;

%
% data
%
output [
  "x: ", show(x), "\n",
  "y: ", show(y), "\n"
];
