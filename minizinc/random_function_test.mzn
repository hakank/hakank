% 
% Random generator in MiniZinc.
% 
% One of the thing I miss in MiniZinc is to be able
% to generate random problem instances. Here's one way...
%
% The seed can be defined via command line via the -D option, e.g.
%   -D "seed=3"
%
%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "random_function.mzn"; 

int: n = 20;
int: mod_value = 10;
int: seed = 1;

% random integers
array[int] of int: rand = random_int(n, seed, mod_value);

% float (not used in the model, just for show)
array[int] of float: rand_float = random_float(n, seed, mod_value);

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
  "rand_float: " ++ show(rand_float) ++ "\n" ++
  "x   : " ++ show(x) ++ "\n" 
 
];

