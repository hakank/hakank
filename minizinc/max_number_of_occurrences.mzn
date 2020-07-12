% 
% Maximum number of occurrences in MiniZinc.
% 
% https://stackoverflow.com/questions/62759396/count-most-frequent-element-in-an-array-in-minizinc
% Count most frequent element in an array in Minizinc
% """
% It's a very simple thing that I want to do in Minizinc. I have an array of integer values,
% and I want to know the number of times that the most common value in it occurs. I can't figure
% out how to do that. I hope someone can help.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

% int: n = 11;
% int: n = 6;
int: n = 7;

% array[1..n] of int: a = [3,3,3,4,4,4,4];
array[1..n] of int: a = [15, 14, 39, 23, 14, 14, 8];

% array[1..n] of var 0..29: a;

% upper value of a
int: upb = ub_array(a);

% Number of occurrences in a
array[0..upb] of var 0..n: gcc;

% max number of occurrenes
var 0..upb: z = max(gcc);

% The value of the max number of occurrences
var 0..upb: max_val = arg_max(gcc)-1;

solve satisfy;

constraint
  % count the number of occurrences in a
  global_cardinality(a, array1d(0..upb,[i | i in 0..upb]), gcc)

  % /\ z > 3
;

output [
        "a: \(a)\n",
        "upb: \(upb)\n",
        "gcc: \(gcc)\n",
        "z: \(z)\n",
        % "max_pos: \(max_pos) = max_val: \(max_val)\n",
        "max_val: \(max_val)\n",        
        "ub_array(a): \(lb_array(a))..\(ub_array(a))\n",
];

