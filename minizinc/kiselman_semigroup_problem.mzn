% 
% Kiselman Semigroup Problem in MiniZinc.
% 
% From the lecture "Constraint Programming" 
% by Alan M. Frisch and Ian Miguel
% http://www-module.cs.york.ac.uk/cop/model1.pdf
% (via http://www-module.cs.york.ac.uk/cop/)
% 
% """
% Given n, a positive integer.
% Find a sequence of integers drawn from 1..n
% Such that between every pair of occurrences of an
% integer i there exists an integer greater than i and an
% integer less than i.
% * If n = 3, a solution is 2, 3, 1, 2
% * We are usually interested in counting the solutions
%   for a given n.
% * This is not a finite domain specification: there are an
%   infinite number of finite sequences
% 
% ....
%
% Given n, a positive integer
%    Find a sequence of integers drawn from 1..n
%      Such that between every pair of occurrences of an integer i
%      there exists an integer greater than i and an integer less than i.
% * Notice:
%   There can be at most 1 occurrence of 1 and n.
%   There can be at most 2 occurrences of 2 and n-1.
%   There can be at most 4 occurrences of 3 and n-2.
% * So, given n, we can derive a maximum sequence
%   length:
% *  For even n: 1+2+4+8+...+ 2^(n/2-1) = 2^(n/2+1)-2
% *  Similarly for odd n.
% *  Hence, domain of the sequence variable is finite.
% """

% Number of solutions for some n
% 
%   n   #solutions
%   1       2
%   2       5
%   3      18
%   4     115
%   5    1710
%   6   83973
%
% This is - not surprisingly - the sequence
%   "Number of elements in the semigroup of type K_n"
% according to The On-Line Encyclopedia of Integer Sequences (OEIS):
% http://oeis.org/A125625
% 
% References for this sequence:
% Kiselman, C.O. (2002). "A Semigroup of Operators in Convexity Theory." 
% Trans. Am, Math. Soc., 354, No. 5, pp. 2035-2053. 
% http://www2.math.uu.se/~kiselman/semigroupTRAN2915.pdf
% 
% Alsaody, S. (2007). "Determining the Elements of a Semigroup." 
% Uppsala, Sweden: Dept. Of Mathematics, Uppsala University, Report No. 2007:3.
% http://www2.math.uu.se/research/pub/Alsaody1.pdf
%

% Experimental: Number of solutions where no 0's are allowed
%   n   #solutions
%   1    1
%   2    2
%   3    2
%   4   14
%   5    8
%   6  838
% 
% Note: This sequence is not in OAIS.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 6;
int: m = ceil(pow(2.0, int2float(n)/2.0+1.0))-2;

% 0's are used as dummy values and must be placed last
array[1..m] of var 0..n: x;

% the length of the sequence
var 0..m: len = sum(i in 1..m) (bool2int(x[i] > 0));

% solve satisfy;
solve :: int_search(x, most_constrained, indomain_min, complete) satisfy;

% The main sequence
constraint
  forall(i in 1..m, j in i+1..m) (
      (x[i] = x[j] /\ x[i] > 0) ->
         (
            exists(k, l in i+1..j-1) (
               x[i] < x[k] /\
               x[i] > x[l]
            )
         )
  )
;

% Zeros, if any, are always last in the sequence.
constraint
  forall(i in 1..m-1) (
     x[i] = 0 -> x[i+1] = 0
  )
;

% test
constraint len = m;


% Experimental: constraint the length of the sequence
% constraint len = m;


output [
  "x: " ++ show(x) ++ " (length: " ++ show(len) ++ ")"
]
++ ["\n"]
;

