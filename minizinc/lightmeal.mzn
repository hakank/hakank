% 
% Lightmeal problem in MiniZinc.
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
%    
%    sum_calories: 5
%    appetiser   : 1 (1: radishes, 2: pasta). Calories: 1
%    main        : 5 (3: beef, 4: pork, 5: sole, 6: tuna). Calories: 2
%    dessert     : 7 (7: fruit, 8: icecream): Calories: 2
%    ----------
%    sum_calories: 7
%    appetiser   : 1 (1: radishes, 2: pasta). Calories: 1
%    main        : 6 (3: beef, 4: pork, 5: sole, 6: tuna). Calories: 4
%    dessert     : 7 (7: fruit, 8: icecream): Calories: 2
%    ----------
%    sum_calories: 8
%    appetiser   : 1 (1: radishes, 2: pasta). Calories: 1
%    main        : 3 (3: beef, 4: pork, 5: sole, 6: tuna). Calories: 5
%    dessert     : 7 (7: fruit, 8: icecream): Calories: 2
%    ----------
%    sum_calories: 10
%    appetiser   : 1 (1: radishes, 2: pasta). Calories: 1
%    main        : 4 (3: beef, 4: pork, 5: sole, 6: tuna). Calories: 7
%    dessert     : 7 (7: fruit, 8: icecream): Calories: 2
%    ----------
%    sum_calories: 9
%    appetiser   : 1 (1: radishes, 2: pasta). Calories: 1
%    main        : 5 (3: beef, 4: pork, 5: sole, 6: tuna). Calories: 2
%    dessert     : 8 (7: fruit, 8: icecream): Calories: 6
%    ----------
%    sum_calories: 10
%    appetiser   : 2 (1: radishes, 2: pasta). Calories: 6
%    main        : 5 (3: beef, 4: pork, 5: sole, 6: tuna). Calories: 2
%    dessert     : 7 (7: fruit, 8: icecream): Calories: 2
%    ----------
%    ==========


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% Identify the dishes
int: radishes = 1;
int: pasta = 2;
int: beef = 3;
int: pork = 4;
int: sole = 5;
int: tuna = 6;
int: fruit = 7;
int: icecream = 8;

% the calories
array[1..8] of int: calories = [1,6,5,7,2,4,2,6];

% decision variables
set of int: v0 = 0..8; % note the 0
set of int: v1 = 1..8; 
var v1: appetiser;
var v0: meat;
var v0: fish;
var v1: main;
var v1: dessert;

% maximum sum of calories
int: max_calories = 10;

% sum of calories of the selected dishes must no exceed max_calories
var 0..sum(i in 1..8) (calories[i]): sum_calories;


solve :: int_search([appetiser, meat, fish, main, dessert, sum_calories], 
        first_fail, indomain_min, complete) satisfy;

constraint

    appetiser in {radishes, pasta}
    /\
    dessert in {fruit, icecream}
    /\ % note: 0 is valid for meat and fish
    meat in {0, beef, pork}
    /\
    fish in {0, sole, tuna}
    /\
    ( % if we select meat we cannot also select fish, and vice versa
      (main = meat /\ fish = 0) 
      \/ 
      (main = fish /\ meat = 0)
     )
    /\
    sum_calories = calories[appetiser] + calories[main] + calories[dessert]
    /\
    sum_calories <= max_calories
;

output[
  "sum_calories: " , show(sum_calories), "\n",
  "appetiser   : ",  show(appetiser), " (1: radishes, 2: pasta). Calories: ", show(calories[appetiser]), "\n",
  "main        : ",  show(main), " (3: beef, 4: pork, 5: sole, 6: tuna). Calories: ", show(calories[main]), "\n",
  "dessert     : ", show(dessert) , " (7: fruit, 8: icecream): Calories: ", show(calories[dessert]), "\n",

];

