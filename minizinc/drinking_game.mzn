% 
% Drinking game problem in Minizinc

% From Marriott & Stuckey "Programming with Constraints", 
% excercise page 184, drinking game
% 
% Must drink if the time is divisible with 5 or 7 if 
% there was no drinking the last 8 minutes.
% 
% Result:
% drinking = [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0]
% number_of_drinks = 10
%
% Model created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%


int: n = 100; % 100 time slots
array[1..n] of var 0..1: drinking; % 1 if drinking time, 0 if not
array[1..n] of var int:  last_8min; % checks the last 8 minutes
var 0..n: num_drinks; 

solve satisfy;

constraint
        forall(t in 1..n) (
           last_8min[t] = sum(i in max(t-8,1)..max(t-1,1)) (drinking[i])
        )
        /\
        forall(t in 1..n) (
           (t mod 5 = 0 \/ t mod 7 = 0) /\ 
           last_8min[t] = 0 <-> drinking[t] = 1
        )
        /\
        num_drinks = sum(t in 1..n) (drinking[t])
;

output
[
  "drinking: " ++ show(drinking) ++ "\n" ++
  "num_drinks: " ++ show(num_drinks) ++ "\n" ++
  % "last_8min: " ++ show(last_8min) ++ "\n" ++
  "drinking times: " ++ show([t | t in 1..n where fix(drinking[t]) = 1])

];