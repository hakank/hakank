% 
% Family riddle in MiniZinc.
% 
% From 
% http://www.comp.rgu.ac.uk/staff/ha/ZCSP/additional_problems/family_riddle/family_riddle.pdf
% """
% Rene and Leo are both heads of households with three boys and girls each. 
% Neither family features any children closer in age than one year (i.e., 
% no twins) and all children are under age ten. The youngest child in Leo’s 
% family is a girl, and in Rene’s household a girl (aged zero) has just 
% been born. 
% In each family, the sum of the ages of the girls equals the 
% sum of the ages of the boys. Likewise, the sums of the squared ages
% of the boys equals the sums of the squared ages of the girls. Summing the 
% ages of all children yields 60 in both families. What are the ages of the 
% children in each family? Table 1 shows the unique solution to this
% question.
%
%                 Leo       Rene
%         girls  3 7 8     0 5 7
%         boys   4 5 9     1 3 8
%
%     Table 1. Solution to the problem
%
% """
% 
% Also, see the Oz presentation and solution:
% http://www.mozart-oz.org/documentation/fdt/node22.html
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
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
  all_different(leo_boys ++ leo_girls) 
  /\
  all_different(rene_boys ++ rene_girls) 

  /\ % youngest is a girl
  minimum(rene_girls[1], rene_boys ++ rene_girls)
  /\
  minimum(leo_girls[1], leo_boys ++ leo_girls)
  /\ % rene has a newborn girl
  rene_girls[1] = 0

  /\ % sums
  sum(rene_boys) = sum(rene_girls)
  /\
  sum(leo_boys) = sum(leo_girls)

  /\ % sum of squares
  sum(i in 1..3) (rene_boys[i]*rene_boys[i]) = sum(i in 1..3) (rene_girls[i]*rene_girls[i])
  /\
  sum(i in 1..3) (leo_boys[i]*leo_boys[i]) = sum(i in 1..3) (leo_girls[i]*leo_girls[i])

  /\ % sum of all is 60
  sum(leo_boys ++ leo_girls ++ rene_boys ++ rene_girls) = 60

  /\ % symmetry breaking
  increasing(leo_boys) /\ increasing(leo_girls)
  /\
  increasing(rene_boys) /\ increasing(rene_girls)
;


output [
  "Leo family : girls: ", show(leo_girls), "  boys: ", show(leo_boys), "\n",
  "Rene family: girls: ", show(rene_girls), "  boys: ", show(rene_boys), "\n",

]
;