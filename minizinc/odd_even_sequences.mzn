% 
% Odd-even sequences in MiniZinc.
% 
% From 
% http://mindyourdecisions.com/blog/2014/04/21/monday-puzzle-how-many-odd-even-sequences-are-there/
% """
% From the numbers 1, 2, … n, start by writing an odd number, and keep writing 
% larger numbers–alternating between even and odd–until you stop at n. Call this an 
% odd-even sequence of n.
% 
% For example, if n = 3, then the only odd-even sequences are 123 and 3.
% 
% If n = 8, then the sequences 1278 and 58 are odd-even, but other sequences are not, 
% like 278 (starts with even number), 138 (two odd numbers in a row), 1438 
% (going from 4 to 3 is a smaller number).
% 
% For an arbitrary n, how many odd-even sequences are there?
% """


% n  #sols
% --------
%  1  1
%  2  1
%  3  2
%  4  3
%  5  5
%  6  8
%  7 13
%  8 21
%  9 34
% 10 55
% 11 89
% 
% (i.e. the Fibonacci number sequence)
 

%
% Note: When I first read the problem, I missed the requirement that 
%       the last number in the sequence should be n
%       ("stop at n" is somewhat ambigous; I read it as "stop when reaching >= n").
%       Here's the number of solutions for that problem, also 
%       Fibonacci related.
% n  #sols
% --------
%  1   1
%  2   2
%  3   4
%  4   7
%  5  12
%  6  20
%  7  33
%  8  54
%  9  88
% 10 143
% 11 232
% 
%
% This is: 1,2,4,7,12,20,33,54,88,143,232
% http://oeis.org/A000071: Fibonacci numbers - 1. 


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 8;

% decision variables
array[1..n] of var 0..n: x;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

predicate strictly_increasing_except_0(array[int] of var int: a) =
    forall(i,j in index_set(a) where i < j) (
       (a[i] != 0 /\ a[j] != 0) -> (a[i] < a[j])
    )
;


constraint
   alldifferent_except_0(x) /\
   % start with odd number
   x[1] mod 2 = 1 
   /\ % last number (> 0) is n
   exists(i in 1..n) (
     x[i] = n /\
     forall(j in i+1..n) (
        x[j] = 0
     )
   )
   /\ % alternating odd/even or 0
   forall(i in 2..n) (
      (x[i] = 0)
      \/ % alternate odd/even numbers
      (x[i] mod 2 = 0 <-> x[i-1] mod 2 = 1)
   )
   /\ % collect 0's last
   forall(i in 1..n) (
      x[i] = 0 -> not(exists(j in i+1..n) (
                        x[j] > 0
                     ))
   )
   /\ strictly_increasing_except_0(x)
;

output [

  show(x)
];

