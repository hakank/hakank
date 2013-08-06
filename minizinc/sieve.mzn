%
% Simple sieve in Minizinc.
% 
% Puts all the divisors in a set and an array.
%
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: n = 123;
int: c = ceil(sqrt(int2float(n)))+2;
array[1..n] of var 0..1: sieve; % 0 -> i is a divisor of n
var set of 1..n: s;

solve satisfy;

constraint
    forall(i in 1..n) (
        (n mod i = 0 <-> sieve[i] = 0)  /\
        (i in s <-> n mod i = 0) % set variant
    )

;


output [
   show(s) ++ "\n" ++
   show(sieve)
];
