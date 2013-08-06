% 
% Five statements problem in MiniZinc.
% 
% From 
% "Puzzle: Joyner’s five statements"
% http://strathmaths.wordpress.com/2012/10/17/puzzle-joyners-five-statements/
% """
% The following little logical teaser appears as “Ponderable 1.1.3″ in 
% David Joyner’s book Adventures in Group Theory (Johns Hopkins University 
% Press, 2008; also available to download for free).
% 
% Determine which of the following statements is true.
% 
%  - Exactly one of these statements is false.
%  - Exactly two of these statements are false.
%  - Exactly three of these statements are false.
%  - Exactly four of these statements are false.
%  - Exactly five of these statements are false.
% 
% (Enthusiasts might like to consider the “natural” generalisation to n statements, 
% and in particular the case n=1…) Thanks to Dr André Sonnet for pointing this 
% one out to me!
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 
int: n = 5;

% decision variables
array[1..n] of var 0..1: x;

solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   forall(i in 1..n) (
      x[i] = 1 <-> sum([bool2int(x[j] == 0) | j in 1..n]) = i
   )

   % if we can assume that at least one statement must be true:
   /\ sum(x) > 0
;

output [
  show(x)
];

