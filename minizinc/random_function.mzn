% 
% Random generator in MiniZinc.
% 
% One of the thing I miss in MiniZinc is to be able
% to generate random problem instances. Here's one way...
%
% The seed can be defined via command line via the -D option, e.g.
%   -D "seed=3"
%
% See random_function_test.mzn for a simple test.
%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

% It probably don't pass many randomness tests but it _looks_ quite random.
function array[int] of int: random_int(int: n, int: seed, int: mod_value) = 
   [ (((seed*104729*i) mod 65537) mod mod_value) | i in 1..n];


function array[int] of float: random_float(int: n, int: seed, int: mod_value) = 
   [ (((seed*104729*i) mod 65537) mod mod_value)/65537.0 | i in 1..n];

