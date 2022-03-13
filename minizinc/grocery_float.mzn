% 
% Grocery problem (using floats) in MiniZinc.
% 
% """
% A kid goes into a grocery store and buys four items. The cashier charges $7.11. 
% The kid pays and is about to leave when the cashier calls the kid back, and says 
% "Hold on, I multiplied the four items instead of adding them; I'll try again... 
% Gosh, with adding them the price still comes to $7.11"! What were the prices of 
% the four items?
% """
%
% The usual approach to this problem is to use integers and multiply
% everything by 100 (see the grocery example in MiniZinc Tutorial.
%
% Here is a version using floats instead. However, we must ensure that we are
% only working with pennies, hence the generated set of floats "pennies".
% This works but it's not fast.
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

% Ensure that we use pennies.
set of float: pennies = { i/100 | i in 1..7*100 } ; 
array[1..4] of var pennies: item;

constraint
  sum(item) = 7.11 /\
  product(item) = 7.11;
  
% symmetry breaking
constraint increasing(item);

% solve satisfy;
solve :: float_search(item,0.01, input_order, indomain_split) satisfy;

output [
   show(item), "\n",
];
