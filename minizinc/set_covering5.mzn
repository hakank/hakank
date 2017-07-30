%
% Set covering, scheduling in MiniZinc.
% 
% Example from the swedish book 
% Lundgren, Rönnqvist, Värbrand "Optimeringslära", page 410.
% 
% Work schedule for a security company with three employees: Anders, Lotta, Per
% (common swedish names).
%
% The tasks has different qualifications, and each person
% may give preferences to each task (each person has 10 points to spread).
%
% Task     Time (h)  Anders           Lotta            Per
%                        Qualify, Points  Qualify, Points  Qualify, Points
%  1             4         X        5      X         4       
%  2             3         X        2                        X       2
%  3             3         X        1      X         2       X       4
%  4             3                         X         2       X       1
%  5             3         X        1                        X       1
%  6             2         X        1                        X       2
%  7             2                         X         1
%  8             2                         X         1
%
% Each person must work 7 or 8 hour per night.
%
% From the table above, 19 different combinations is construed, given at
% page 412. See the data section for the combinations.
%
% The problem is now to assign one combination to each person so
% that all tasks is covered exactly once, and maximizing the total
% preference points.
%
% The answer from the book, page 413: 
% The combination is 1 (Anders), 13 (Lotta), 18 (Per) with the following
% tasks:
%   Anders: 1,2    Lotta: 4,7,8    Per: 3,5,6
%
% The total preference points is 18.
% 
%
% The following solutions is returned with solve satisfy.
% There are two solutions with total preference points of 18.
%
% The employee and his/her combinations:
%    anders: 1..6, lotta: 7..13, per: 14..19
%
% The solutions
%      1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19                   
%
% Solution 1:
% x = [0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0]
% comb:      3                         12       15
% tasks      3: 1,5  12: 3,7,8   15: 2,4,6   ALL!
% points        5+1       2+1+1       2+1+2  = 6+4+4  = 14  
%
% Solution 2:
% x = [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0]
% comb:   2                               13       16
% tasks: 2: 1,3   13: 4,7,8   16: 2,5,6  ALL!
% points:         5+1       2+1+1       2+1+2 = 6+4+5 = 15    
%
% Solution 3:
% x = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1]
% comb 1                               12                   19
% tasks:   1: 1,2    12: 3,7,8   19: 4,5,6  ALL!     
% points:         5+2        2+1+1       1+1+2 = 7+4+4= 15
%
% Solution 4:
% x = [0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0]
% comb:      3                            13 14
% tasks: 3: 1,5    13: 4,7,8     14:2,3,6  ALL!
% points:         5+1        2+1+1        2+4+2 = 6+4+8 = 18
%
% Solution 5:
% x = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0]
% comb:1                                  13             18
% tasks: 1: 1,2    13:4,7,8    18: 3,5,6   ALL!
% points          5+2       2+1+1        4+1+2   = 7 + 4 + 7 = 18
%
% Solution 5 is the one given in the book, but solution 4 is also
% optimal. Solution 5 is slighly fairer than solution 4, 
% since it gives equal points to Anders and Per.
% 
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: num_combinations; % the combinations
int: num_tasks;  % the tasks

% costs for the combinations. Not used...
% array[1..num_combinations] of int: costs; 

% the combinations, and their tasks
array[1..num_combinations] of set of 1..num_tasks: c; 

int: num_persons;
% which person per combination
array[1..num_combinations] of 1..num_persons: combinations_person;

% the preferences for a task
array[1..num_persons, 1..num_tasks] of int: preferences;

% decision variable: which alternative to choose
array[1..num_combinations] of var 0..1: x; 

% This is the same as preference_points
% var int: total_cost = sum(i in 1..num_combinations) (x[i]*costs[i]);

set of int: preference_range; % range of preference points

% the total preference points per person fullfilled in the task
% (larger is better)
array[1..num_persons] of var preference_range: preference_points;

% the objective to optimize: total of preference points, 
var int: sum_pref_points = sum(preference_points);

solve satisfy;
% solve maximize sum_pref_points; % satisfy the employee's preferences


constraint

 % sum_pref_points >= 18 % for solve satisfy
 % /\

 % choose one combination for each person
 forall(p in 1..num_persons) (
   sum(j in 1..num_combinations where combinations_person[j] = p) (
        x[j]
   ) = 1
   /\ % calculate the preference points for the person
   preference_points[p] = 
      sum(i in 1..num_combinations, j in 1..num_tasks where combinations_person[i] = p) (
        x[i]* bool2int(j in c[i])* preferences[p,j]
      )
 )
 /\ % all tasks must be covered exactly once
 forall(j in 1..num_tasks) (
     sum(i in 1..num_combinations) (
        x[i] * bool2int(j in c[i])
    ) = 1
 )

;

%
% data
%
num_combinations =  19;

% costs is total points score for each combination (listed in the book).
% I use the preference table below instead, since it seems
% somewhat more general.
% costs = [7,6,6,4,4,3,6,6,6,5,5,4,4,8,5,5,7,7,4]; % "cost" for a combination
num_tasks = 8;
num_persons = 3;

% which combination belongs to which person
% 1: Anders, 2: Lotta, 3: Per
combinations_person = 
 [
  % Anders, comb. 1..6
  1, 1, 1, 1, 1, 1,

  % Lotta, comb. 7..13
  2, 2, 2, 2, 2, 2, 2,

  % Per, comb., 14..19
  3, 3, 3, 3, 3, 3
];

preference_range = 1..10;

% preference point per tasks
preferences = 
[| 
  % 1 2 3 4 5 6 7 8
    5,2,1,0,1,1,0,0, % Anders
    |4,0,2,2,0,0,1,1, % Lotta
    |0,2,4,1,1,2,0,0, % Per
|];

% the combinations and the tasks they contain (set representation)
c = [
  % Anders
  {1,2},   % 1
  {1,3},   % 2
  {1,5},   % 3
  {2,3,6}, % 4
  {2,5,6}, % 5
  {3,5,6}, % 6 

  % Lotta
  {1,3},   % 7
  {1,4},   % 8
  {1,7,8}, % 9
  {3,4,7}, % 10
  {3,4,8}, % 11
  {3,7,8}, % 12
  {4,7,8}, % 13

  % Per
  {2,3,6}, % 14
  {2,4,6}, % 15
  {2,5,6}, % 16
  {3,4,6}, % 17
  {3,5,6}, % 18 
  {4,5,6}  % 19
];

output 
[ 
  "x: ", show(x), "\n",
  "preference_points: ", show(preference_points), "\n",
  "sum_pref_points: ", show(sum_pref_points), "\n"

];