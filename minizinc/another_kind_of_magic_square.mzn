% 
% Another kind of magic square in MiniZinc.
% 
% From
% http://benvitale-funwithnum3ers.blogspot.com/2010/12/another-kind-of-magic-square.html
% """
% We all know this type of Magic Square ....
%
% 8 1 6
% 3 5 7
% 4 9 2
%
% where the numbers are arranged such that the sum of the numbers in any 
% horizontal, vertical, or main diagonal line is always the same number
%
% 8 + 1 + 6 = 8 + 3 + 4 = 8 + 5 + 2 = 15
%
% For normal magic squares of order n = 3, 4, 5, ..., the magic constants 
% are: n(n^2+1)/2
%
% http://oeis.org/A006003
%
% http://mathworld.wolfram.com/MagicSquare.html
%
% Now, let's consider a magic square where the numbers 1 to 9 in a 3x3 array so 
% that the numbers surrounding each number add to a multiple of that number.
%
% 2 6 5
% 7 3 1
% 9 8 4
%
% Notice that
% 7 + 3 + 6 = 16 (a multiple of 2)
% 6 + 3 + 1 = 10 (a multiple of 5)
% 8 + 3 + 1 = 12 (a multiple of 4)
% 8 + 3 + 7 = 18 (a multiple of 9)
% 
% """
%
% Notes:
%   1) this problem don't have the constraint that all rows, columns,
%      and diagonals must sum to the same value.
%      So it is "Another kind" of magic squares.
%   2) Number of solutions for certain n
%         n = 2: 0 solutions
%         n = 3: 8 solutions
%         n = 4: 0 solutions It took about 2:24 minutes and 9551717 failures 
%                            to prove this in Gecode/fz.
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

int: n = 3; 
array[1..n,1..n] of var 1..n*n: magic;


% solve satisfy;
solve :: int_search(
        [magic[i,j] | i in 1..n, j in 1..n], 
        largest, 
        indomain_min,
        complete) 
    satisfy;


constraint
       all_different([magic[i,j] | i in 1..n, j in 1..n]) :: domain
;

constraint
    forall(i,j in 1..n) (
        (sum(a,b in {-1,0,1} where 
            i+a > 0  /\ j+b >  0 /\
            i+a <= n /\ j+b <= n
         ) (
             magic[i+a,j+b]
            )
         ) mod magic[i,j] = 0 :: bounds
      )
;


output [
      show(magic[i,j]) ++ " " ++ if j mod n == 0 then "\n" else "" endif 
      | i,j in 1..n
];
