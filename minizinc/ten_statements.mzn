% 
% Ten statements problem in MiniZinc.
% 
% From Richard Wiseman
% http://richardwiseman.wordpress.com/2012/07/20/its-the-friday-puzzle-169/
% """
% Here are 10 numbered statements.  How many of them are true?
%
% 1) Exactly one of these statements is true.
% 2) Exactly two of these statements are true.
% 3) Exactly three of these statements are true.
% 4) Exactly four of these statements are true.
% 5) Exactly five of these statements are true.
% 6) Exactly six of these statements are true.
% 7) Exactly seven of these statements are true.
% 8) Exactly eight of these statements are true.
% 9) Exactly nine of these statements are true.
% 10) Exactly ten of these statements are true.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 
int: n = 10;


% decision variables
array[1..n] of var 0..1: x;

solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   forall(i in 1..n) (
      x[i] = 1 <-> sum(x) = i
   )

   % if we can assume that at least one statement must be true:
   /\ sum(x) > 0
;

output [
  show(x)
];

