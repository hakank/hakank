% 
% Guards and apple problem in MiniZinc.
% 
% https://gist.github.com/2484152
% """
% <masak> a boy wants to give an apple to a girl. to get to her, he has to 
% pass through five gates, each with a guard. he bribes each guard with 
% half of his apples, plus one. after he's given the apple to the girl, 
% he has no apples left. how many did he have to begin with?
% """


% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
int: n = 5;
array[1..n+1] of var 0..1000: x;

solve satisfy;

constraint
  x[n+1] = 1 % apples left
  /\ % 1..n guards
  forall(i in 1..n) (
    %  x[i] = (1+x[i+1])*2
    x[i+1] = (x[i] div 2) - 1
  )
;

output [
  "x: " ++ show(x) ++ "\n" ++
  "start: " ++ show(x[1]) ++ "\n"
]
 ++ ["\n"]
;

