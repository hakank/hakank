% Langford's number problem in MiniZinc.
%
% This model is based on the ESSENCE' model in the Minion Translator examples:
%
% http://www.cs.st-andrews.ac.uk/~andrea/examples/langford/langford.eprime
% """
% Langford's number problem (CSP lib problem 24)
%
% Arrange 2 sets of positive integers 1..k to a sequence,
% such that, following the first occurence of an integer i, 
% each subsequent occurrence of i, appears i+1 indices later
% than the last. 
% For example, for k=4, a solution would be 41312432
% """
% 
% Also see: http://www.csplib.org/prob/prob024/
%
% However, I added a better representation were we see the numbers in their
% proper positions: the solution array.
%
% For a generalized version (>= 2 sets), see 
% http://hakank.org/minizinc/langford_generalized.mzn
%
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% Solution for k = 4 (with symmetry breaking)
% [2, 3, 4, 2, 1, 3, 1, 4]
%

include "globals.mzn";

int: k;
set of int: positionDomain = 1..2*k;

array[positionDomain] of var positionDomain: position;
% better presentation:
array[positionDomain] of var 1..k: solution;

solve :: int_search(
        position, 
        first_fail, 
        indomain_min, 
        complete) satisfy;

constraint 
    assert( k mod 4 == 0 \/ k mod 4 = 3, "k mod 4 = 0 or k mod 4 = 3")
;

constraint
  forall(i in 1..k) (
     position[i+k] = position[i] + i+1 /\
     % hakank: added this
     solution[position[i]] = i /\ 
     solution[position[k+i]] = i
  )
  /\   
  all_different(position)
  /\ % symmetry breaking
  solution[1] < solution[2*k]
;


output [
  % "position: ", show(position), "\n",
  "solution: ", show(solution), "\n"
];

%
% data
%


% k mod 4 = 0 \/ k mod 4 = 3
%
k = 4;
% k = 7;
% k = 8;
% k = 11;
% k = 12;
% k = 15;
% k = 16;
% k = 19;
% k = 20;
% k = 23;
% k = 24;
% k = 27;
% k = 35;

