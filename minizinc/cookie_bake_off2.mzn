% 
% Cookie Bake Off problem (PuzzlOr) in MiniZinc.
% 
% http://puzzlor.editme.com/Cookiebakeoff
% """
% Historically, baking has been considered an art rather than a science.  
% As a contestant in a cookie bake-off, your attempts to create the perfect cookie have 
% failed.  For your final attempt, you decide to use your analytical skills to analyze 
% your past attempts to see if you can find the recipe for the perfect cookie.
% Table 1 shows your past 15 attempted recipes, each with varying amounts of 
% Sugar, Flour, and Butter, and a corresponding judge’s score for each recipe.  
% The judge’s scores range from 0 (worst) to 100 (best).  None of your recipes so 
% far have achieved the coveted perfect score of 100.  All recipes sum up to 
% 8 cups total of the three ingredients.
% 
% Batch Sugar 	Flour 	Butter 	Score
% 1 	3 	4 	1 	70
% 2 	1 	4 	3 	95
% 3 	2 	2 	4 	45
% 4 	2 	1 	5 	20
% 5 	2 	3 	3 	85
% 6 	3 	2 	3 	55
% 7 	2 	5 	1 	80
% 8 	1 	1 	6 	15
% 9 	1 	5 	2 	90
% 10 	3 	3 	2 	75
% 11 	4 	2 	2 	40
% 12 	1 	3 	4 	65
% 13 	1 	6 	1 	60
% 14 	4 	1 	3 	25
% 15 	4 	3 	1 	50
% 
% Table 1
% 
% Question:  Given your past recipes and scores, which recipe below has the best chance 
% of achieving a perfect score of 100?
%    a.) 1 cup sugar; 2 cups flour; 5 cups butter
%    b.) 2 cups sugar; 4 cups flour; 2 cup butter
%    c.) 3 cups sugar; 1 cup flour; 4 cups butter
% 
% Send your answer to puzzlor@gmail.com by February 15th, 2013. 
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

int: n = 15;

array[1..n, 1..4] of float: scores =
array2d(1..n, 1..4,[
3.0, 4.0, 1.0, 70.0, 
1.0, 4.0, 3.0, 95.0, 
2.0, 2.0, 4.0, 45.0, 
2.0, 1.0, 5.0, 20.0, 
2.0, 3.0, 3.0, 85.0, 
3.0, 2.0, 3.0, 55.0, 
2.0, 5.0, 1.0, 80.0, 
1.0, 1.0, 6.0, 15.0, 
1.0, 5.0, 2.0, 90.0, 
3.0, 3.0, 2.0, 75.0, 
4.0, 2.0, 2.0, 40.0, 
1.0, 3.0, 4.0, 65.0, 
1.0, 6.0, 1.0, 60.0, 
4.0, 1.0, 3.0, 25.0, 
4.0, 3.0, 1.0, 50.0, 
]);

array[1..3, 1..3] of float: scores_test = 
array2d(1..3, 1..3,
[
1.0, 2.0, 5.0,
2.0, 4.0, 2.0,
3.0, 1.0, 4.0,
]);

% decision variables
array[1..3] of var 0.0..100.0: x;
% array[1..n] of var -100.0..1000.0: diffs;

% G12/mip can't handle abs() so we have to split it 
% into diffs+ and diffs-
array[1..n] of var -100.0..0.0: diffs_minus;
array[1..n] of var 0.0..1000.0: diffs_plus;

% var 0.0..1000.0: diffs_sum = sum(i in 1..n) (abs(diffs[i]));
var 0.0..1000.0: diffs_sum = sum(i in 1..n) (diffs_plus[i]+diffs_minus[i]);

% solve minimize diffs_sum;
solve :: float_search(x++diffs_minus++diffs_plus, 0.001,input_order, indomain_split, complete) minimize diffs_sum;

constraint
   forall(i in 1..n) (
      % scores[i, 4] = sum(j in 1..3) (x[j]*scores[i,j])+diffs[i]     
      scores[i, 4] = sum(j in 1..3) (x[j]*scores[i,j])+diffs_plus[i]+diffs_minus[i]
   )
;

output [
  "x: " ++ show(x) ++ "\n" ++
  % "diffs: " ++ show(diffs) ++ "\n" ++
  "diffs_plus: " ++ show(diffs_plus) ++ "\n" ++
  "diffs_minus: " ++ show(diffs_minus) ++ "\n" ++
  "diffs_sum: " ++ show(diffs_sum) ++ "\n"
]
++
[
  % "test " ++ show(t) ++ ": " ++ show(sum(i in 1..3) (scores_test[t,i]*fix(x[i]))) ++ "\n" % MiniZinc 2.0 complains about fix/1
  "test " ++ show(t) ++ ": " ++ show(sum(i in 1..3) (scores_test[t,i]*x[i])) ++ "\n"
  | t in 1..3
]
;

