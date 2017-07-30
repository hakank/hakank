% 
% Combination Locks problem (PuzzlOR) in MiniZinc.
% 
% From http://puzzlor.editme.com/combinationlocks
% """
% Many people store their valuables in home safes because they 
% protect against burglaries and fires.  They are a good place for 
% storing insurance documents, car titles, cash, and many other valuables.
% 
% Figure 1 shows six dials that are on the front of your home safe.  
% In order to open the safe, you must set each of the dials to one number.  
% When the correct numbers are selected on each dial, the safe will open.  
% Unfortunately you have forgotten the combination.  All you can remember 
% is that the numbers on all of the dials summed to 419.
% 
% Question:  What numbers should you select in order to unlock the safe?
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
int: n = 6;
array[1..n] of set of int: dials = [
  {39,57,15,88,75,06},
  {09,64,48,68,58,02},
  {29,91,08,67,16,55},
  {40,25,32,22,66,54},
  {49,30,14,41,17,01},
  {44,03,46,83,10,63},
];

% decision variables
array[1..n] of var 0..max([max(dials[i]) | i in 1..n ]): x;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  forall(i in 1..n) (
      x[i] in dials[i]
  )
  /\
  sum(x) = 419
;

output [
  show(x)
];

