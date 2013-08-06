% 
% Farm puzzle in MiniZinc.
% 
% "Talk" version. See farm_puzzle.mzn for a different version.
% 
% From http://www.cs.st-andrews.ac.uk/~andrea/tailor/examples.html
% """
% A farmer has 7 animals on his farm: pigs and hens.
% They all together have 22 legs. How many pigs (4 legs)
% and how many hens (2 legs) does the farmer have?
% """
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
int: number_of_legs = 22;
set of int: feet = {2,4};

var feet: animal1;
var feet: animal2;
var feet: animal3;
var feet: animal4;
var feet: animal5;
var feet: animal6;
var feet: animal7;

solve satisfy;

constraint
     animal1 + animal2 + animal3 + animal4 + animal5 + animal6 + animal7 = number_of_legs
;

output
[
 "number_of_legs: " ++ show(number_of_legs) ++ "\n" ++
 show([animal1, animal2, animal3, animal4, animal5, animal7, animal7])

];