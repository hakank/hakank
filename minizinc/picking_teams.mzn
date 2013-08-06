% 
% Picking teams in MiniZinc.
% 
% This model was inspired by David Curran's
% blog post "The Fairest Way to Pick a Team "
% http://liveatthewitchtrials.blogspot.se/2012/06/fairest-way-to-pick-team.html
% """
% What is the best way to pick a team? As kids we would always strictly alternate 
% between teams so team 1 had first team 2 the second pick and then team 1 again etc.
% 
% Most things you can measure about people are on a bell curve. A small number of 
% people are bad, most are in the middle and a few are good. There are a few good 
% known metrics of ability. None are perfect, there is no one number that can sum up 
% ability. The simpler the sport the more one metric can tell you, in cycling VO2 max is 
% a very good indicator. Whereas in soccer VO2 max, kicking speed, vertical leap, number 
% of keep me ups you can do etc could all measure some part of football ability.
% 
% So say there was one good metric for a task and teams were picked based on this. 
% Is the standard strict alteration, where Team 1 picks then Team 2 alternating, fair? 
% Fair here meaning both teams end up with a similar quality. 
% """
% 
% For n = 10, where s = 1..n there are 20 optimal solutions with a diff of 1
% (with the symmetry breaking that x[1] is in team 1).
% 
% Example:
%  
% x: [1, 2, 2, 2, 1, 1, 1, 1, 2, 2]
% diff: 1
% team1: [1, 5, 6, 7, 8]  sum: 27
% team2: [2, 3, 4, 9, 10]  sum: 28
% ----------
% x: [1, 2, 2, 2, 1, 1, 1, 2, 1, 2]
% diff: 1
% team1: [1, 5, 6, 7, 9]  sum: 28
% team2: [2, 3, 4, 8, 10]  sum: 27
%

% The strengths 1..100 is solved fast with occurrence/indomain_random.
% Though 1..102 and 1..104 is much harder.

%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

% int: n = 100;
% Each person's "strength"
% array[1..n] of int: s = [i | i in 1..n];

% This is no problem
int: n = 100;
array[1..n] of int: s = [35, 52, 17, 26, 90, 55, 57, 54, 41, 9, 75, 24, 17, 23, 62, 74, 100, 67, 40, 48, 7, 6, 44, 19, 16, 14, 2, 66, 70, 2, 43, 45, 76, 53, 90, 12, 88, 96, 30, 30, 36, 93, 74, 1, 52, 45, 38, 7, 24, 96, 17, 21, 12, 12, 23, 90, 77, 64, 37, 79, 67, 62, 24, 11, 74, 82, 51, 17, 72, 18, 37, 94, 43, 44, 32, 86, 94, 33, 97, 27, 38, 38, 29, 92, 35, 82, 22, 66, 80, 8, 62, 72, 25, 13, 94, 42, 51, 31, 69, 66];

% decision variables
% The team to belong
array[1..n] of var 1..2: x;

% The difference in strength between the teams
var 0..sum(s) div 2: d;

% solve minimize d;
solve :: int_search(
        x, 
        occurrence, 
        indomain_random, 
        complete) 
    minimize d;
    % satisfy;

% for solve satisfy (n=10)
% constraint d = 1; 

constraint
  assert(n mod 2 == 0, "n must be even")
;

constraint
  % the difference in strength between the teams
  d = abs(sum([s[i]*bool2int(x[i] == 1) | i in 1..n]) - 
          sum([s[i]*bool2int(x[i] == 2) | i in 1..n]))

  /\ % same size of team
  count(x, 1, n div 2) :: domain

  /\ % symmetry breaking: assign first person to team 1
  x[1] = 1

  % divisibility of the sum
  /\ d mod 2 = sum(s) mod 2 
;

output 
[
  if n <= 100 then
    "s: " ++ show(s) ++ "\n"
  else 
    ""
  endif
]
++
[
  "n: " ++ show(n) ++ "\n" ++
  "sum(s): " ++ show(sum(s)) ++ "\n" ++
  "x: " ++ show(x) ++ "\n" ++ 
  "diff: " ++ show(d) ++ "\n"
]
++
[
  "team" ++ show(j) ++ ": " ++ show([s[i] | i in 1..n where fix(x[i]) == j]) ++ 
   "  sum: " ++ show(sum([s[i] | i in 1..n where fix(x[i]) == j])) ++ "\n"
  | j in 1..2
]
;

