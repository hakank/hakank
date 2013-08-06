% 
% Lightmeal problem (integer programming version) in MiniZinc.
% 
% Integer programming version and more general than lightmeal.mzn.
% 
%
% From Mark Wallace: "Constraint Programming" 
% http://eclipse-clp.org/reports/handbook/node14.html
% """
% We shall use an example from [A. Colmerauer. 
% "An introduction to Prolog III", 1990] to illustrate how it works.
% 
%    Given the definition of a meal as consisting of an appetiser, a main 
%    meal and a dessert, and given a database of foods and their calorific 
%    values, we wish to construct light meals i.e. meals whose total 
%    calorific value does not exceed 10. 
% 
% ...
%
% A CLP(R) program for solving this problem is shown in figure.
%
%   lightmeal(A, M, D) :-
%      I>0, J>0, K>0,
%      I+J+K <= 10,
%      appetiser(A,I),
%      main(M,J),
%      dessert(D, K).
%
%   main(M,I) :-
%      meat(M,I).
%   main(M,I) :-
%      fish(M,I).
%
%   appetiser(radishes, 1).
%   appetiser(pasta, 6).
%   meat(beef, 5).
%   meat(pork, 7).
%   fish(sole, 2).
%   fish(tuna,4).
%   dessert(fruit, 2).
%   dessert(icecream, 6).
%
% """


% 
% Answer:
%    sum_calories: 10
%    x: [0, 1, 0, 0, 1, 0, 1, 0]
%    ----------
%    sum_calories: 7
%    x: [1, 0, 0, 0, 0, 1, 1, 0]
%    ----------
%    sum_calories: 9
%    x: [1, 0, 0, 0, 1, 0, 0, 1]
%    ----------
%    sum_calories: 5
%    x: [1, 0, 0, 0, 1, 0, 1, 0]
%    ----------
%    sum_calories: 10
%    x: [1, 0, 0, 1, 0, 0, 1, 0]
%    ----------
%    sum_calories: 8
%    x: [1, 0, 1, 0, 0, 0, 1, 0]
%    ----------
%    ==========

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: n = 8;
% the calories
array[1..n] of int: calories   = [1,6,5,7,2,4,2,6]; 
array[1..n] of int: appetisers = [1,6,0,0,0,0,0,0];
array[1..n] of int: meat       = [0,0,5,7,0,0,0,0];
array[1..n] of int: fish       = [0,0,0,0,2,4,0,0];
array[1..n] of int: dessert    = [0,0,0,0,0,0,2,6];

% decision variables
array[1..n] of var 0..1: x;

% maximum sum of calories
int: max_calories = 10;

% sum of calories of the selected dishes must no exceed max_calories
var 0..sum(i in 1..n) (calories[i]): sum_calories;

solve :: int_search(x ++ [sum_calories], first_fail, indomain_min, complete) satisfy;

constraint
   sum_calories = sum(i in 1..n) (
      bool2int(appetisers[i] > 0) * appetisers[i] * x[i] + 
      bool2int(meat[i]       > 0) * meat[i]       * x[i] + 
      bool2int(fish[i]       > 0) * fish[i]       * x[i] + 
      bool2int(dessert[i]    > 0) * dessert[i]    * x[i]
   )

   %/\ % main is either meat of fish
   %   % This constraint is not necessary since it is covered below.
   % forall(i in 1..n) (
   %   (meat[i] = 0 \/ fish[i] = 0)
   % )

   /\ % must choose exactly one of the different type of meal
   sum(i in 1..n) ( bool2int(appetisers[i] > 0) * x[i])  = 1 /\ % one appetiser
   sum(i in 1..n) ( bool2int(meat[i]       > 0) * x[i]) <= 1 /\ % either meat
   sum(i in 1..n) ( bool2int(fish[i]       > 0) * x[i]) <= 1 /\ % or fish
   sum(i in 1..n) ( bool2int(dessert[i]    > 0) * x[i])  = 1    % one dessert
   /\ % exactly three dishes
   sum(i in 1..n) (x[i]) = 3

   /\
   sum_calories <= max_calories
;

output[
  "sum_calories: " , show(sum_calories), "\n",
  "x: " , show(x), "\n",
];

