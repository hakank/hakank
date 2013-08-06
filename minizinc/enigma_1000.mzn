% 
% Enigma puzzle 1000 in MiniZinc.
% 
% Problem from ECLiPSe example enigma1000.pl:
% """
% Enigma 1000 by Richard England (New Scientist 10/10/98)
%
%    ENIGMA / M = TIMES
%    Each letter stands for a different digit.
%    No number starts with zero.
% """

% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

set of int: Range = 0..9;
var Range: E;
var Range: N;
var Range: I;
var Range: G;
var Range: M;
var Range: A;
var Range: T;
var Range: S;
array[1..8] of var Range: letters = [E,N,I,G,M,A,T,S];

solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint

   E > 0 /\ M > 0 /\ T > 0
   /\
   all_different(letters)
   /\
   100000*E + 10000*N + 1000*I + 100*G + 10*M + A
	= M * (10000*T + 1000*I + 100*M + 10*E + S)
;

output
[
  show(letters)
];
