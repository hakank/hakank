% 
% Number theoretic problem in MiniZinc.
% 
% From God Plays Dice:
% "A cute number theory puzzle"
% http://godplaysdice.blogspot.com/2011/07/cute-number-theory-puzzle.html
% """
% Today's MAA number of the day, when multiplied by its successor, 
% gives a number concatenated with itself. (Like 455 * 539 = 245245, 
% except 539 isn't the successor of 455.)
% 
% Puzzle: find all such three-digit numbers. (The fact that I'm 
% phrasing it this way should indicate to you that there's more than one.)
% 
% Meta-puzzle: generalize this. (I have some generalizations in mind.)
% """
%
% The problem is explained here:
% "Solution to a puzzle from a few months ago "
% http://godplaysdice.blogspot.com/2011/10/solution-to-puzzle-from-few-months-ago.html
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
var 0..999: x;
array[1..3] of var 0..9: x_a;

var 0..999: y;
array[1..3] of var 0..9: y_a;

var 0..999999: z;
array[1..6] of var 0..9: z_a;


predicate toNum(array[int] of var int: a, var int: n) =
          let { int: len = length(a) }
          in
          n = sum(i in 1..len) (
            ceil(pow(10.0, int2float(len-i))) * a[i]
          )
          /\ forall(i in 1..len) (a[i] >= 0)
;


% solve satisfy;
solve :: int_search(x_a, most_constrained, indomain_min, complete) satisfy;

constraint
   toNum(x_a, x) /\
   toNum(y_a, y) /\
   toNum(z_a, z) /\

   y = x + 1 /\
   x * y = z /\
   forall(i in 1..3) ( 
      z_a[i] = z_a[i+3]
   )
;

output [
  show(x)  ++ " * " ++ show(y)  ++ " = " ++ show(z)  ++ "\n"
]
 ++ ["\n"]
;

