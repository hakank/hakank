%
% Knapsack (investment) problem in MiniZinc.
%
% From the swedish book
%
% Lundgren, Rönnqvist, Värbrand "Optimeringslära", page 393ff.
% 
% A company shall invest in some building projects with the following
% limits:
%
%  - budget of 225 Mkr (million swedish kronor)
%  - 28 persons available
%  - maximum 9 projects can be selected
%  - some project may not be selected together with other projects, and some
%    projects must be selected together with other.
% 
% (I'm keeping the swedish object names.)
%
% No.  Object   Value(kkr) Budget(Mkr) Personell  Not with  Requires
% 1  Ishall      600        35            5        10        -
% 2  Sporthall   400        34            3        -         -
% 3  Hotell      100        26            4        -         15
% 4  Restaurang  150        12            2        -         15
% 5  Kontor A     80        10            2        6         -
% 6  Kontor B    120        18            2        5         -
% 7  Skola       200        32            4        -         -
% 8  Dagis       220        11            1        -         7
% 9  Lager        90        10            1        -         -
% 10 Simhall     380        22            5        1         -
% 11 Hyreshus    290        27            3        15        -
% 12 Bilverkstad 130        18            2        -         -
% 13 Tennishall   80        16            2        -         2
% 14 Idrottsanl. 270        29            4        -         2
% 15 Båthamn     280        22            3        11        -
% 
%
% Solution (page 395): 
% The following project is selected
%   1,2,4,6,7,8,12,14,15
% and optimal value is 2370kkr.
%

%
% This MiniZinc model uses a more general model than the book's model.
%
% Note: The proper way should be to set x as a bool array, but there is
% some problems with that approach:
%   - as of now the ECLiPSe eplex solver cannot handle the problem
%   - Gecode/flatzinc is very slow
%   - we must use alot of bool2int:s for converting booleans to ints.
%
% The minizinc solver, as well as ECLiPSe ic, fd gives the 
% correct answer, though.
%
% The solution is the same as the book (well, we must check,
% mustn't we? :-)
%
% x = [1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1]
%      1  2     4     6  7  8          12    14 15
% total_values = 2370
% total_projects = 9
% total_persons = 26
% total_budget = 211

% Question: Is there another solution with total_values = 2370?
% Change to solve satisfy and test...
% Answer: No, that's the unique solution.
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


int: num_projects; % number of projects to select from
int: max_budget;   % budget limit 
int: max_persons;  % persons available
int: max_projects; % max number of projects to select

% the values of each project
array[1..num_projects] of int: values;
array[1..num_projects] of int: budgets;
array[1..num_projects] of int: personell;

% project i cannot be selected with project j
int: num_not_with;
array[1..num_not_with, 1..2] of 1..num_projects: not_with;

% project i requires project j 
int: num_requires;
array[1..num_requires, 1..2] of 1..num_projects: requires;

% decision variable: what project to select
array[1..num_projects] of var 0..1: x;


var int: total_persons  = sum(i in 1..num_projects) (x[i]*personell[i]);
var int: total_budget   = sum(i in 1..num_projects) (x[i]*budgets[i]);
var int: total_projects = sum(i in 1..num_projects) (x[i]);

% the objective to maximize
var int: total_values   = sum(i in 1..num_projects) (x[i]*values[i]);

% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) maximize total_values;
% solve maximize total_values;

constraint
   % total_values >= 2370 /\ % for solve satisfy

   % resource limits:
   total_budget <= max_budget
   /\
   total_persons <= max_persons
   /\
   total_projects <= max_projects

   %
   % special requirements, using standard integer programming "tricks"
   %
   /\ % projects that require other projects
   forall(i in 1..num_requires) (
      x[requires[i, 1]] - x[requires[i, 2]] <= 0
      % x[requires[i, 1]] -> x[requires[i, 2]] % x as bool
   )
   /\ % projects excluding other projects
   forall(i in 1..num_not_with) (
      x[not_with[i, 1]] + x[not_with[i, 2]] <= 1
      % x[not_with[i, 1]] -> not x[not_with[i, 2]] % x as bool
   )
;

%
% data
%
num_projects = 15;
max_budget = 225;
max_projects = 9;
max_persons = 28;
values = [600,400,100,150, 80,120,200,220, 90,380,290,130, 80,270,280];
budgets = [35,34,26,12,10,18,32,11,10,22,27,18,16,29,22];

num_not_with = 6;
not_with = array2d(1..num_not_with, 1..2, [ 
  1, 10,
  5, 6,
  6,5,
  10, 1,
  11, 15,
  15, 11
]);

num_requires = 5;
requires = array2d(1..num_requires, 1..2, [
  3, 15,
  4, 15,
  8, 7,
  13, 2,
  14, 2
]);

personell = [5,3,4,2,2,2,4,1,1,5,3,2,2,4,3];


output
[
  "x: " ++ show(x) ++ "\n" ++ 
  "total_persons: " ++ show(total_persons) ++ "\n" ++ 
  "total_budget: " ++ show(total_budget) ++ "\n" ++ 
  "total_projects: " ++ show(total_projects) ++ "\n" ++ 
  "total_values: " ++ show(total_values) ++ "\n"
 
];
