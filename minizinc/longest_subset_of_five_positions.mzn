/*
  Longest subset of five-positions five-elements permutations in MiniZinc.
 
  Longest subset of five-positions five-elements permutations, only one-element-position in common
  https://stackoverflow.com/questions/62100291/longest-subset-of-five-positions-five-elements-permutations-only-one-element-po
"""
  I am trying to get the longest list of a set of five ordered position, 1 to 5 each, 
  satisfying the condition that any two members of the list cannot share more than one 
  identical position (index). I.e., 11111 and 12222 is permitted (only the 1 at index 
  0 is shared), but 11111 and 11222 is not permitted (same value at index 0 and 1).

  I have tried a brute-force attack, starting with the complete list of permutations, 
  3125 members, and walking through the list element by element, rejecting the ones 
  that do not match the criteria, in several steps:

     step one: testing elements 2 to 3125 against element 1, getting a new shorter list L'
     step one: testing elements 3 to N' against element 2', getting a shorter list yet L'',

  and so on.

  I get a 17 members solution, perfectly valid. The problem is that:

  - I know there are, at least, two 25-member valid solution found by a matter 
    of good luck, 
    The solution by this brute-force method depends strongly on the initial order 
  of the 3125 members list, so I have been able to find from 12- to 21-member solutions, 
  shuffling the L0 list, but I have never hit the 25-member solutions.

  Could anyone please put light on the problem? Thank you.

  This is my approach so far
  [Python code]
  """

  
  This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my MiniZinc page: http://www.hakank.org/minizinc/

*/

include "globals.mzn"; 

int: n = 5;
int: m = 25;

array[1..m,1..n] of var 1..n: x;

solve satisfy;

constraint
  forall(i in 1..m, j in i+1..m) (
   % at most 1 common element in same position
    sum([x[i,k] = x[j,k] | k in 1..n]) <= 1
  )
  /\  %symmetry breaking (ordered sub sequence)
  lex2(x)
;  

output [
  "\(x[i,..])\n"
 | i in 1..m
];

