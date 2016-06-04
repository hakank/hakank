% 
% Archery puzzle in MiniZinc.
% 
% http://www.eonhq.com/m/puzzles/images/archery-puzzle.jpg
% Archery puzzle by Sam Loyd:
% """
% How close can the young archer come to scoring a total of
% 100 - using as many arrows as she please.
% [The targets are: 16, 17, 23, 24, 39, 40]
% """
% Via: The Aperiodical: "Manchester MathsJam June 2012 Recap"
% http://aperiodical.com/2012/06/manchester-mathsjam-june-2012-recap/

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

int: n = 6;
array[1..n] of int: targets = [16, 17, 23, 24, 39, 40];

int: target = 100;

% decision variables
array[1..n] of var 0..10: x;

var int: z = sum([x[i]*targets[i] | i in 1..n]);
var int: diffz;

% solve minimize diffz;
solve :: int_search(x, occurrence, indomain_max, complete) minimize diffz;

constraint
  diffz = abs(target-z)
;

output [
  "targets: ", show(targets), "\n",
  "x: ", show(x),"\n",
  "z: ", show(z), "\n",
  "diffz: ", show(diffz),"\n\n"
]
++
[
 if fix(x[i]) > 0 then 
   "\(fix(x[i])) hits on score \(targets[i])\n"
 else 
   ""
 endif
 | i in 1..n where fix(x[i]) > 0
];


