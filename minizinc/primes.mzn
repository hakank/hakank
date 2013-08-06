%
% Prime numbers in Minizinc
% 
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";
int: m = 120;
var 1..m: nn;

var 0..m: num_divisors;
% int: t = (n div 2)+1; % number of elements in the divisors array
var set of 1..m: divisor; 
var 0..1: is_prime;


%
% Calculate the number of divisors of n
%
predicate divisors(var int: n, var int: num_divisors, var set of int: d) =
   let { 
         int: ubn = ub(n), 
         int: t = (ubn div 2)+1
       } 
   in 
    n > 1 /\
    % is a divisor?
    forall(i in 2..t) (
        (i in d <-> n mod i = 0) 
        % /\
        % (not(i in d) <-> n mod i > 0)
     ) 
    /\
    forall(i in t+1..ubn-1) (
        not(i in d)
    )
    /\
    1 in d 
    /\
    n in d
    /\ 
    num_divisors = card(d)
;

% solve satisfy;
solve :: set_search([divisor], first_fail, indomain_min, complete) satisfy;

constraint
   % nn = 100 /\
   divisors(nn, num_divisors, divisor)
   /\
   (is_prime = 1 <-> num_divisors = 2) /\
   (is_prime = 0 <-> num_divisors > 2)

   % /\ is_prime = 1
;


output [
"nn: " ++ show(nn) ++ "\n"++
"num_divisors: " ++ show(num_divisors) ++ "\n"++
"divisor: " ++ show(divisor) ++ "\n"++
"is_prime: " ++ show(is_prime) ++ "\n"
];
