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
% This is a MIP approach.
%
% For a CP version, see
%   http://www.hakank.org/minizinc/farmer_and_cows.mzn
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
array[1..num_cows, 1..num_sons] of var 0..1: cows;

solve satisfy;
% solve :: int_search(sons, first_fail, indomain_split, complete) satisfy;

constraint
   sum(sons) = total_milk 
   /\
   forall(i in 2..num_sons) (
      sons[i] = sons[i-1]
   )
   /\ % amount of milk per son
   forall(s in 1..num_sons) (
      sons[s] = sum([c*cows[c,s] | c in 1..num_cows])
   )
   /\ % a cow is given to one son
   forall(c in 1..num_cows) (
      sum([cows[c,s] | s in 1..num_sons]) = 1
   )
   /\ % each son get 9 cows
   forall(s in 1..num_sons) (
      sum([cows[c,s] | c in 1..num_cows]) = num_sons
   )
;


output [
  "sons: " ++ show(sons) ++ "\n" 
   % ++ "cows: " ++ show(cows) ++ "\n"
]
++
[  
   "son " ++ show(s) ++ " got these cows: " ++ 
      show([c | c in 1..num_cows where fix(cows[c,s]) = 1]) ++ "\n"
   | s in 1..num_sons
]
% ++
% [ if s = 1 then "\n" else " " endif ++ 
%    show(cows[c,s])
%   | c in 1..num_cows, s in 1..num_sons
% ]
;

