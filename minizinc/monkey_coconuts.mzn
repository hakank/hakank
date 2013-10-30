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

% Also, see
% Gary Antonick (Wordplay at New York Times)
% "Martin Gardner’s The Monkey and the Coconuts"
% http://wordplay.blogs.nytimes.com/2013/10/07/gardner-2/
%

% Solutions for different n according to this model
%  n  coconuts
%  -----------
%  2    7
%  3   79
%  4  1021
%  5 15621
%  6 279931
%  7 5764795
%  8 134217721
% 
% From OEIS:
% 7,79,1021,15621,279931,5764795,134217721
% http://oeis.org/A014293
% """
% A014293 		n^(n+1)-n+1. 
% Solution to the classical "Monkey and Coconut Problem" 
% for n sailors.
%
% Also called "Sailors and Monkey Problem": a(n) is smallest 
% number such that can apply C -> (C-1)(1-1/n) n times and at 
% every step have an integer C = 1 mod n. 
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

int: n; % = 3; % number of men

array[0..n] of var int: left;
array[1..n+1] of var int: removed;



solve satisfy;
% solve :: int_search(left ++ removed, "first_fail", "indomain", "complete") satisfy;
% solve :: int_search(left ++ removed, first_fail, indomain_median, complete) minimize left[0];
% solve minimize left[0];

constraint 
  forall(i in 0..n)   ( left[i] >= 0 ) 
  /\
  forall(i in 1..n+1) ( removed[i] >= 0 ) 
  /\
  forall(i in 0..n-1) ( 
     left[i] = n*removed[i+1] + 1 
     /\ 
     left[i+1] = (n-1)*removed[i+1]
  ) 
  /\
  left[n] = n*removed[n+1] + 1
;


output [
  "original #: " ++ show(left[0]) ++ "\n" ++
  "left: " ++ show(left) ++ "\n" ++
  "removed: " ++ show(removed) ++ "\n"

];
