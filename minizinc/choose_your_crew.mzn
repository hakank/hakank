% 
% Choose your crew (PuzzlOR problem) in MiniZinc.

% By John Toczek
% From 
% http://www.informs.org/ORMS-Today/Public-Articles/April-Volume-38-Number-2/THE-PUZZLOR
% """
% Choose your crew
%
% Successfully navigating the waters during sea voyages is a 
% challenging task. A captain’s most important decision is selecting 
% the right crew for the voyage. A mix of different skill sets is 
% required to sail the ship efficiently, navigate to the destination 
% and fish for good along the way.
%
% Table 1 shows a list of crew members available for you to 
% hire for the voyage. Each crew member demands a salary for the 
% voyage and has different skill levels of fishing, sailing and navigation.
%
% In order for your journey to be successful, you must have a 
% cumulative skill of 15 or more in each of the three skill 
% categories from all of your chosen crew members. You may choose 
% as many crew members as you like.
%
% [Table 1
%    Name   Fishing  Sailing Navigation   Salary (in 1000)
%    -------------------------------------------
%     Amy    3        5         1          46
%     Bill   1        2         5          43
%     Carl   3        4         2          47
%     Dan    4        3         1          36
%     Eva    4        2         2          43
%     Fred   1        3         4          55
%     Greg   3        1         5          68
%     Henry  5        4         2          64
%     Ida    3        3         3          60
%
% What is the minimum achievable cost of the voyage?
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 9;
int: num_skills = 3;


array[1..n] of string: names = ["Amy" , "Bill", "Carl", "Dan", "Eva", "Fred", "Greg", "Henry", "Ida"];

array[1..n, 1..num_skills] of int: skills = array2d(1..n, 1..3, 
                                     [3,5,1,
                                      1,2,5,
                                      3,4,2,
                                      4,3,1,
                                      4,2,2,
                                      1,3,4,
                                      3,1,5,
                                      5,4,2,
                                      3,3,3]);

array[1..n] of int: salary = [46,43,47,36,43,55,68,64,60];

int: min_limit = 15;

% decision variables
array[1..n] of var 0..1: x;
var 0..sum(salary): total_cost = sum(i in 1..n) (x[i]*salary[i]);

array[1..num_skills] of var 0..sum([skills[i,j] | i in 1..n, j in 1..num_skills]): sum_skills =
         [sum([skills[i,j]*x[i] | i in 1..n]) | j in 1..num_skills];  

% solve satisfy;
solve minimize total_cost;
% solve :: int_search(x, first_fail, indomain_min, complete) minimize total_cost;

constraint
   forall(skill in 1..num_skills) (
       sum(i in 1..n) (x[i]*skills[i,skill]) >= min_limit
   )
;

% for solve satisfy
% constraint total_cost <= 268;

output [
  "total cost: " ++ show(total_cost) ++ "\n" ++
  "x: " ++ show(x)
] ++ 
["\n"] ++ [

   if fix(x[i] = 1) then 
      show(names[i]) ++ "\t" ++ show(salary[i]) ++ "   " ++
      show(skills[i, 1]) ++ "  " ++ 
      show(skills[i, 2]) ++ "  " ++ 
      show(skills[i, 3]) ++ "\n"
   else 
      ""
   endif
   | i in 1..n
] ++ [
   "Sum skills: "
] ++ [
  show(sum_skills[j]) ++ " "
  | j in 1..num_skills
] ++ ["\n"];


