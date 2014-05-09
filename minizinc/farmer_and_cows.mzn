% 
% Farmer and cow problem in MiniZinc.
% 
% From 
% http://stackoverflow.com/questions/23153447/confused-how-to-implement-mathematics-in-c
% """
% A farmer has 81 cows numbered 1 to 81. no 1 cow gives 1kg milk ... 
% no 2 gives 2 kg...... no 81 gives 81 kg per day. farmer has 9 sons. 
% now he wants to distribute his cows among his sons such a way that 
% every son take 9 cows & total quantity of milk will be same. 
% how can he distribute?
% """
%
% The total quantity each son get for different number of sons:
%   #sons   total quantinity
%     1        1
%     2        5
%     3       15
%     4       34
%     5       65
%     6      111
%     7      175
%     8      260
%     9      369
%    10      505
%    11      671
%    12      870
% 
% 
% 1,5,15,34,65,111,175,260,369,505,671,870
% This is integer sequence https://oeis.org/A006003: n*(n^2 + 1) / 2
% """
% Write the natural numbers in groups: 1; 2,3; 4,5,6; 7,8,9,10; ... 
% and add the groups. In other words, "sum of the next n natural numbers".
% ...
% The sequence M(n) of magic constants for n X n magic squares 
% (numbered 1 through n^2) from n=3 begins M(n) = 15, 34, 65, 111, 175, 260, ..
% """
%
% See 
%   http://www.hakank.org/minizinc/sum_of_next_natural_numbers.mzn
% for some exploration on these sequences.
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: num_sons = 9; 
int: num_cows = num_sons*num_sons;

% total quantity of milk to distrubute 1+2...+81
int: total_milk = (num_cows+1)*num_cows div 2; 

% decision variables

% quantity of milk for a son's cow
array[1..num_sons] of var num_cows..num_cows*num_sons: sons; 


% which son got which cow?
array[1..num_cows] of var 1..num_sons: cows; 

% solve satisfy;
solve :: int_search(sons, first_fail, indomain_split, complete) satisfy;

constraint
  sum(sons) = total_milk  

  /\ % all sons' quantity of milk should be the same
  forall(i in 2..num_sons) (
    sons[i] = sons[i-1]
  )

  /\ % quantity of milk per son
  forall(i in 1..num_sons) (
     sons[i] = sum([c*bool2int(cows[c] = i) | c in 1..num_cows])
  )
  /\ % each son get 9 cows
  forall(s in 1..num_sons) (
       % sum([bool2int(cows[c] == s) | c in 1..num_cows]) = num_sons
       count(cows, s, num_sons)
  )  
  % /\ global_cardinality(cows, [i | i in 1..num_sons], [num_sons | i in 1..num_sons]) 

  /\ % symmetry breaking
  cows[1] = 1
;

output [
  "sons: " ++ show(sons) ++ "\n" 
   ++ "cows: " ++ show(cows) ++ "\n"
]
++
[
  "son " ++ show(s) ++ " get these cows: " ++ show([c | c in 1..num_cows where fix(cows[c] = s)]) ++ "\n"
  | s in 1..num_sons
]
;

