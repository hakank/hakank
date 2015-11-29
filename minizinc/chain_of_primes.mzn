% 
% Chain of Primes (Enigma #1216) in MiniZinc.
% 
% https://enigmaticcode.wordpress.com/2015/07/22/enigma-1216-chain-of-primes/
% """
% From New Scientist #2371, 7th December 2002

% I have constructed a chain of ten 2-digit prime numbers. The ten 
% primes that I have used are all different and except in the case 
% of the first prime in the chain each prime’s first digit is the same as 
% the previous prime’s second digit. In addition the fourth prime is the 
% reverse of the first prime, and the tenth prime is the reverse of 
% the seventh prime.
% 
% What (in this order) are the third, sixth and ninth primes in this chain?
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 10;

% primes between 11..99
int: max_val = 99;
set of int: primes = 11..max_val diff { i | i in 2..max_val, j in 2..ceil(sqrt(i))+1 where i mod j = 0};

% decision variables
array[1..n] of var 11..99: x; % 2 digit numbers

% x is y reversed
predicate reversed(var int: x, var int: y) =
  x mod 10 = y div 10 /\ x div 10 = y mod 10 
;

solve satisfy;

constraint
  all_different(x) /\
  forall(i in 1..n) ( x[i] in primes ) /\ 
  forall(i in 2..n) ( x[i] div 10 = x[i-1] mod 10 ) /\
  reversed(x[4],x[1]) /\
  reversed(x[7],x[10])
;

output [ show(x) ];

