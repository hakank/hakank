%  
% Age problem in MiniZinc.
%
% Problem from CTK Insights
% "When Two Wrongs Make One Right"
% http://www.mathteacherctk.com/blog/2012/04/when-two-wrongs-make-one-right/
% """
% John is twice as old as Jim; the sum of their ages is that of 
% Jerry's, while the ages of the three of them add up to 36. 
% How old is each? 
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

var 0..36: john;
var 0..36: jim;
var 0..36: jerry;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint

   % John is twice as old as Jim
   john = jim*2 

   %  the sum of their ages is that of Jerry's, 
   /\
   john + jim = jerry

   % while the ages of the three of them add up to 36. 
   /\
   john + jim + jerry = 36


;

output [
  "john: " ++ show(john) ++ "\n" ++
  "jim: " ++ show(jim) ++ "\n" ++
  "jerry: " ++ show(jerry) ++ "\n"
]
 ++ ["\n"]
;

