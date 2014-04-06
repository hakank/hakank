% 
% Random generator in MiniZinc.
% 
% One of the thing I miss in MiniZinc is to be able
% to generate random problem instances. Here's one way...
%
% This is a simpler version than 
%    http://www.hakank.org/minizinc/random_generator.mzn
% which has the advantage of being able to use directly in the
% array definition, i.e. it don't have to involve an array of var ints.
%
% The same principle can be used to generate floats.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

int: n = 20;
int: max_value = 10;
int: seed = 1;

% It probably don't pass many randomness tests but it _looks_ quite random.
array[1..n] of int: rand = [ (((seed*104729*i) mod 65537) mod max_value) | i in 1..n];

% "random" floats
array[1..n] of float: rand_float = [ int2float((seed*104729*i) mod 65537)/65537.0 | i in 1..n];


% decision variables

array[1..n] of var int: x;

% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   % just a silly test
   forall(i in 1..n-1) (
     x[i] >= 0 /\
     x[i+1]+x[i] = (rand[i]+rand[i+1])
   )   
;

output [
  "rand: " ++ show(rand) ++ "\n" ++
  "x   : " ++ show(x) ++ "\n" ++ 
  "rand_float (not used in the model): " ++ show(rand_float) ++ "\n"
 
];

