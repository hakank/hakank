% 
% Fibonacci numbers in Minizinc
%
% Since recursion is not allowed in Minizinc, we use arrays instead.
%
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: n = 11; % number of Fibonacci numbers to calculate
array[1..n] of var int: fib;

solve satisfy;

constraint
        fib[1] = 1 /\
        fib[2] = 1 /\
        forall(i in 3..n) (
             fib[i] = fib[i-1] + fib[i-2]
        )        
;


output [
  show(fib), "\n"
];