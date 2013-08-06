% 
% Collatz problem in MiniZinc
% 
% http://en.wikipedia.org/wiki/Collatz_conjecture:
% """
% t(x) = x/2 if x is even,
% t(x) = (3x+1)/2 if x is odd,
% """
%
% Checking a Collatz sequence:
% http://www.numbertheory.org/php/collatz.html
%
%
% Note: 
% The biggest problem here is to calculate the length of the array x for a given n.
% Since MiniZinc requires to fix the array length at "compile time", this model will 
% not work for all n (in the solver's normal integer range). 
%


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
 
int: n = 117;
int: max_len = 3*n;

% decision variables
% Note: after 1 is reached the first time (at length len) then the
%       values toggles between 1 and 2.
array[1..max_len] of var int: x;
var 0..max_len: len; % the length of the sequence until (first) reaching 1


solve :: int_search(x, first_fail, indomain_split, complete) satisfy;

predicate collatz(var int: x, var int: y) =
      (x mod 2 = 0 -> y = x div 2) /\
      (x mod 2 = 1 -> y = ((3*x)+1) div 2)
;

constraint
   x[1] = n
   /\
   forall(i in 2..max_len) (
      x[i] >= 1 
      /\
      x[i-1] >= 1
      /\
      collatz(x[i-1], x[i])
      % /\ x[i] <= n*n*n % upper bound for ECLiPSe/ic
   )

   /\
   exists(i in 1..max_len) (
      x[i] = 1
      /\
      len = (i-1) /\ forall(j in 1..i-1) (x[j] > 1)
   )

   % /\
   % x = [5, 8, 4, 2, 1] % n = 5
   %  x = [12, 6, 3, 5, 8, 4, 2, 1] %  n = 12
   % x = [6, 3, 5, 8, 4, 2, 1] % n = 6
;


output [
   "n: " ++ show(n) ++ "\n" ++
   "len: " ++ show(len) ++ "\n" ++
   "x: " ++ show(x) ++ "\n"
];
