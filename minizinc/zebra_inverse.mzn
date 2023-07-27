% 
% Zebra puzzle using the inverse constraint in MiniZinc.
% 
% From Global Constraint Catalog
% https://sofdem.github.io/gccat/gccat/Kzebra_puzzle.html
%
% This model uses the inverse constraint instead of all_different.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

var int: red; 
var int: green; 
var int: ivory; 
var int: yellow; 
var int: blue;

var int: englishman; 
var int: spaniard; 
var int: ukrainian; 
var int: norwegian; 
var int: japanese;

var int: dog; 
var int: snail; 
var int: fox; 
var int: horse; 
var int: zebra;

var int: coffee; 
var int: tea; 
var int: milk; 
var int: orange_juice; 
var int: water;

var int: old_gold; 
var int: kool; 
var int: chesterfield; 
var int: lucky_strike; 
var int: parliament;

array[1..5] of var 1..5: Color;
array[1..5] of var 1..5: Nationality;
array[1..5] of var 1..5: Animal;
array[1..5] of var 1..5: Drinks;
array[1..5] of var 1..5: Brand;

solve satisfy;

constraint

   inverse(Color,[red, green, ivory, yellow, blue]) /\
   inverse(Nationality, [englishman, spaniard, ukrainian, norwegian, japanese]) /\
   inverse(Animal, [dog, snail, fox, horse, zebra]) /\
   inverse(Drinks, [coffee, tea, milk, orange_juice, water]) /\
   inverse(Brand, [old_gold, kool, chesterfield, lucky_strike, parliament]) /\

   englishman = red            /\ % (the Englishman lives in the red house).
   spaniard = dog              /\ % (the Spaniard owns the dog).
   coffee = green              /\ % (coffee is drunk in the green house).
   ukrainian = tea             /\ % (the Ukrainian drinks tea).
   green = ivory+1             /\ % (the green house is immediately to the right of the ivory house).
   old_gold = snail            /\ % (the Old Gold smoker owns snails).
   kool = yellow               /\ % (kools are smoked in the yellow house).
   milk = 3                    /\ % (milk is drunk in the middle house).
   norwegian = 1               /\ % (the Norwegian lives in the first house).
   abs(chesterfield-fox) = 1   /\ % (the man who smokes Chesterfields lives in the house next to the man with the fox).
   abs(kool-horse) = 1         /\ % (kools are smoked in the house next to the house where the horse is kept).
   lucky_strike = orange_juice /\ % (the Lucky Strike smoker drinks orange juice).
   japanese = parliament       /\ % (the Japanese smokes Parliaments).
   abs(norwegian-blue) = 1        % (the Norwegian lives next to the blue house).

;

output 
[
"color      : ", show([red, green, ivory, yellow, blue]), "\n",
"nationality: ", show([englishman, spaniard, ukrainian, norwegian, japanese]), "\n",
"animal     : ", show([dog, snail, fox, horse, zebra]), "\n",
"drinks     : ", show([coffee, tea, milk, orange_juice, water]), "\n",
"brand      : ", show([old_gold, kool, chesterfield, lucky_strike, parliament]), "\n"
] 
;
