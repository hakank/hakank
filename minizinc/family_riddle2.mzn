% 
% Family riddle (v 2) in MiniZinc.
% 
% From https://dmcommunity.org/challenge/challenge-june-2016/
% """
% Let’s assume that Rene and Leo are both heads of household, and, what a coincidence, both families include three 
% girls and three boys. The youngest child in Leo’s family is a girl, and in Rene’s family, a little girl has 
% just arrived. In other words, there is a girl in Rene’s family whose age is less than one year. Neither family 
% includes any twins, nor any children closer in  age than a year. All the children are under age ten. In each family, 
% the sum of the ages of the girls is equal to the sum of the ages of the boys. The sum of the ages of all 
% these children is 60.
% 
% Question: What are the ages of the children in these two families?
% 
% Send your solutions using any BR&DM tool (or none) to DecisionManagementCommunity@gmail.com.
% """

% Note: There is a similar - but not identical - problem solved in http://hakank.org/minizinc/family_riddle.mzn
% That problem has a unique solution, whereas the current problem has 266 solutions (since the sum of squares
% constraint has been removed, see below).

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
array[1..3] of var 0..9:  leo_boys;
array[1..3] of var 0..9:  leo_girls;

array[1..3] of var 0..9:  rene_boys;
array[1..3] of var 0..9:  rene_girls;

% solve satisfy;
solve :: int_search(leo_boys ++ leo_girls ++ rene_boys ++ rene_girls, first_fail, indomain_min, complete) satisfy;

constraint
  % all different ages (in each family)
  all_different(leo_boys ++ leo_girls) /\
  all_different(rene_boys ++ rene_girls) /\ 

  % youngest is a girl
  minimum(rene_girls[1], rene_boys ++ rene_girls) /\
  minimum(leo_girls[1], leo_boys ++ leo_girls) /\

  % rene has a newborn girl
  rene_girls[1] = 0 /\

  % sums
  sum(rene_boys) = sum(rene_girls) /\
  sum(leo_boys) = sum(leo_girls) /\

  % sum of squares 
  % Note: these constraints where included in the family_riddle.mzn problem but is not in the current formulation.
  % sum(i in 1..3) (rene_boys[i]*rene_boys[i]) = sum(i in 1..3) (rene_girls[i]*rene_girls[i]) /\
  % sum(i in 1..3) (leo_boys[i]*leo_boys[i]) = sum(i in 1..3) (leo_girls[i]*leo_girls[i])

  % sum of all children's ages is 60
  sum(leo_boys ++ leo_girls ++ rene_boys ++ rene_girls) = 60 /\

  % symmetry breaking
  increasing(leo_boys) /\ increasing(leo_girls) /\
  increasing(rene_boys) /\ increasing(rene_girls)
;


output [
  "Leo family : girls: ", show(leo_girls), "  boys: ", show(leo_boys), "\n",
  "Rene family: girls: ", show(rene_girls), "  boys: ", show(rene_boys), "\n",

]
;
