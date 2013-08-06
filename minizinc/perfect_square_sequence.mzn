% 
% Perfect square sequence in MiniZinc.
% 
% From "Fun with num3ers"
% "Sequence"
% http://benvitale-funwithnum3ers.blogspot.com/2010/11/sequence.html
% """
% If we take the numbers from 1 to 15 
%     (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15) 
% and rearrange them in such an order that any two consecutive 
% numbers in the sequence add up to a perfect square, we get,
% 
% 8     1     15     10     6     3     13     12      4      5     11     14   %    2      7      9
%     9    16    25     16     9     16     25     16     9     16     25     16%      9     16
% 
% 
% I ask the readers the following:
% 
% Can you take the numbers from 1 to 25 to produce such an arrangement?
% How about the numbers from 1 to 100?
% """
% 
% Via http://wildaboutmath.com/2010/11/26/wild-about-math-bloggers-111910
% 
% Compare with the following model:
% * Google CP Solver: http://www.hakank.org/google_or_tools/perfect_square_sequence.py
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
int: n = 15;

% array[1..n] of var 1..n: x  :: viz([
%                                               % viztype("vector_waterfall"),
%                                               viztype("vector"),
%                                               vizpos(0,2),
%                                               % vizdisplay("compact"),
%                                               vizdisplay("expanded"),
%                                               vizwidth(n),
%                                               vizheight(n),
%                                               vizrange(1,n)
%                                               ]);

array[1..n] of var 1..n: x;
set of int: squares = { i*i  | i in 1..ceil(sqrt(int2float(n*n)))};

predicate square(var int: y) =
  exists(z in 1..ceil(sqrt(int2float(ub(y))))) (
        z*z = y
  )
;


% solve satisfy;
solve :: int_search(x, max_regret, indomain_split, complete) satisfy;

constraint
  trace("n:" ++ show(n) ++ "\n", 1=1) /\

  all_different(x) 
  /\

  forall(i in 2..n) (
     % square(x[i-1]+x[i]) 
     (x[i-1]+x[i]) in squares
  )

  /\ % symmetry breaking
  x[1] < x[n]
;

output [
  "x" ++ show(x) ++ "\n"

] ++ ["\n"];