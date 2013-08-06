% 
% The Monkey and the Coconuts in MiniZinc.
% 
% http://mathworld.wolfram.com/MonkeyandCoconutProblem.html
% """
% Given n men and a pile of coconuts, each man in sequence takes (1/n)th of the 
% coconuts left after the previous man removed his (i.e., a_1 for the first man, 
% a_2, for the second, ..., a_n for the last) and gives m coconuts 
% (specified in the problem to be the same number for each man) which do not 
% divide equally to a monkey. When all n men have so divided, they divide the 
% remaining coconuts n ways (i.e., taking an additional a coconuts each), and 
% give the m coconuts which are left over to the monkey. If m is the same at 
% each division, then how many coconuts N were there originally? 
% """
% 
% Answer: The original amount of coconuts is in left[0].

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

int: n = 5; % number of men

array[0..n] of var int: left;
array[1..n+1] of var int: removed;



solve satisfy;
% solve :: int_search(left ++ removed, "first_fail", "indomain", "complete") satisfy;
% solve minimize left[0];

constraint 
  forall(i in 0..n)   ( 
        left[i] >= 0 
  ) 
  /\
  forall(i in 1..n+1) ( 
        removed[i] >= 0 
  ) 
  /\
  forall(i in 0..n-1) ( 
        left[i] = n*removed[i+1] + 1 /\ left[i+1] = (n-1)*removed[i+1]
  ) 
  /\
  left[n] = n*removed[n+1] + 1
;


output [
  "left: " ++ show(left) ++ "\n" ++
  "removed: " ++ show(removed) ++ "\n"

];
