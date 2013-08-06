% 
% Harry Potter's potion problem in MiniZinc.
% 
% 
% http://cse.csusb.edu/dick/cs320/prolog/Potions.htm
% """
% In the first "Harry Potter" book, Harry and Hermione are trapped in a room 
% with a table that has 7 different bottles on it. They have to find the 
% bottle to let them move forward through the room and the bottle that 
% lets them go back.
% 
% On the table is a roll of parchment that reads:
%
%     Danger lies before you, while safety lies behind,
%     Two of us will help you, whichever you would find.
%     One among us seven will let you move ahead,
%     Another will transport the drinker back instead.
%     Two among our number hold only nettle wine,
%     Three of us are killers, waiting hidden in line.
%     Choose, unless you wish to stay here forevermore,
%     To help you in your choice, we give you these clues four:
%
%     First, however slyly the poison tries to hide
%     You will always find some on nettle wine's left side;
%
%     Second, different are those who stand at either end,
%     But if you would move forward, nether is your friend;
%
%     Third, as you see clearly, all are different size,
%     Neither dwarf nor giant holds death in their insides;
%
%     Fourth, the second left and second on the right
%     Are twins once you taste them, though different at first sight.
%
% Hermione remarks: "This isn't magic -- it's logic." and soon works out which 
% is the one that takes a person foraward and which will take a person back. Can you?
% """
% 
% This model is based on the corresponding Prolog version:
% http://cse.csusb.edu/dick/cs320/prolog/potion1.plg
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 7;

int: wine = 1; int: poison  = 2; int: forward = 3; int: back    = 4;

set of int: s = {wine,poison,forward,back};
array[1..4] of string: ss = ["wine", "poison", "forward", "back"];

% decision variables
var s: p1; var s: p2; var s: p3; var s: p4;
var s: p5; var s: p6; var s: p7; 
var s: dwarf; var s: giant;
array[1..n] of var s: x = [p1,p2,p3,p4,p5,p6,p7];

solve satisfy;

constraint

 % one forward, one backward, 2 wines, and 3 poisons in some order
 global_cardinality(x, [forward,back,wine,poison], [1,1,2,3]) /\

 % Look at the drawing for the biggest and smallest
 dwarf = p3 /\ giant = p2 /\

 % First clue -- Each wine bottle has poison on the left
 p1 != wine /\
 forall(i in 2..n) ( x[i] = wine -> x[i-1] = poison ) /\

 % Second clue -- the ends are different
 p1 != p7 /\

 % and neither end moves you forward
 p1 != forward /\ 
 p7 != forward /\

 % Third clue -- The largest and smallest are not poison
 dwarf != poison /\ giant != poison /\

 % Fourth clue -- second left and second on the right are the same
 p2 = p6
;

output [
  show(x) ++ "\n" ++ 
  show([ ss[fix(x[i])] | i in 1..n ]) ++ "\n"
];

