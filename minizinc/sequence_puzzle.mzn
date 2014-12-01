% 
% Sequence puzzle in MiniZinc.
% 
% From 
% http://stackoverflow.com/questions/27089772/whats-a-more-efficient-implementation-of-this-puzzle
% """
% The puzzle
%
% For every input number n (n < 10) there is an output number m such that:
%
%     m's first digit is n
%     m is an n digit number
%     every 2 digit sequence inside m must be a different prime number
%
% The output should be m where m is the smallest number that fulfils the conditions above. 
% If there is no such number, the output should be -1;
%
% Examples
%
% n = 3 -> m = 311
%
% n = 4 -> m = 4113 (note that this is not 4111 as that would be repeating 11)
%
% n = 9 -> m = 971131737
% """

%
% Here are solutions for n=2..9
% 
% x: [1]
% p: []
% m: 1
%
% x: [2, 3]
% p: [23]
% m: 23
%
% x: [3, 1, 1]
% p: [31, 11]
% m: 311
%
% x: [4, 1, 1, 3]
% p: [41, 11, 13]
% m: 4113
%
% x: [5, 3, 1, 1, 3]
% p: [53, 31, 11, 13]
% m: 53113
%
% x: [6, 1, 1, 3, 1, 7]
% p: [61, 11, 13, 31, 17]
% m: 611317
%
% x: [7, 1, 1, 3, 1, 7, 3]
% p: [71, 11, 13, 31, 17, 73]
% m: 7113173
%
% x: [8, 3, 1, 1, 3, 7, 1, 7]
% p: [83, 31, 11, 13, 37, 71, 17]
% m: 83113717
%
% x: [9, 7, 1, 1, 3, 1, 7, 3, 7]
% p: [97, 71, 11, 13, 31, 17, 73, 37]
% m: 971131737

%
% For larger n (> 9) then the requirement "m's first digit is n" makes no sense.
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n; %  = 4;

% 2 digit primes
set of int: primes = {11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97};

% decision variables
array[1..n] of var 0..9: x;
var pow(10,n-1)..pow(10,n)-1: m;
array[1..n-1] of var primes: p;

% solve minimize m;
solve :: int_search(x, first_fail, indomain_min, complete) minimize m;

predicate to_num(array[int] of var int: a, var int: n) =
  let { int: len = length(a) }
  in
  n = sum(i in 1..len) (  
        pow(10, len-i) * a[i]
      )
;

constraint x[1] = n;
constraint to_num(x,m);
constraint
  forall(i in 1..n-1) (
    % each 2-digit sequence is a prime
    to_num([x[j] | j in i..i+1], p[i])
  )
;

% all primes are distinct
constraint all_different(p); 

output [
  "x: ", show(x), "\n",
  "p: ", show(p), "\n",
  "m: ", show(m), "\n",
];

