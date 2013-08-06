% 
% Circular chain - Enigma puzzle 985 in MiniZinc.
%
% Problem from ECLiPSe example enigma985.pl 
% """
%  Enigma 985 by Colin Singleton (New Scientist 27/6/98)
%
%    George has invested his savings in gold - a circular chain of eight
%    linked gold rings.  The rings are all different sizes, although
%    each is a whole number of ounces in weight - the total is 57
%    ounces.
%    The chain has been designed so that the first time that George
%    wishes to sell some of his gold, he can remove any whole number of
%    ounces from the chain, either as a single link, or as a chain of
%    linked rings.
%    Given that there is no 3-ounce ring, nor a 6-ounce ring, list the
%    weights of the eight rings in order, starting with the two smallest.
%
% """
 

% Solution: [1, 2, 10, 19, 4, 7, 9, 5]
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: n = 8;
int: m = 57;
array[1..n] of var 1..m: chain;
var int: chain_sum = sum(chain);

solve :: int_search(chain, smallest, indomain_split, complete) satisfy;
% solve satisfy;

constraint
     chain_sum = m
     /\
     forall(i in 1..n) (
       chain[i] != 3  /\ chain[i] != 6
     )
     /\ 
     chain[1] = 1
     /\
     chain[2] < chain[n]
     /\
     all_different(chain) 

     /\ % for all numbers s in 1..m some subsequence sums to i
     forall(s in 1..m) (
       exists(i, j in 1..n) (
         % either a straight subsequence
         (sum(c in i..j) (chain[c]) = s )

         \/ % or a subsequence wrapped around the corner
         (sum(c in i..n) (chain[c]) + sum(c in 1..j) (chain[c]) ) = s 
       )
     )
;

output
[
  "chain: " ++ show(chain)
];
