/*
  Permutation symmetry in MiniZinc.

  https://stackoverflow.com/questions/66385872/minizinc-constraint-programming-enforce-interchangeability-of-items
  """
  Minizinc (Constraint programming) enforce interchangeability of items

  I'm using Minizinc to do constraint programming.

  The problem is as follows:

  I have an array of colors, denoted as ints. So this looks like: 
     [3, 5, 5, 2, 1, 3, 1].

  This array should be optimized in such a way that the changes of 
  colors is minimized.

  An optimal solution for example would be: [1, 1, 3, 3, 2, 5, 5].

  I also have an order array, which is used to optimize with. This 
  array denotes the sequence in which the coloring will be done. So 
  the value of each x_i in the order array, denotes the index in the 
  optimal sequence. So the optimal solution, the order array would be:

    [7, 5, 1, 6, 4, 2, 3]

  For this small example it is pretty trivial how to write the proper 
  constraints. However, if N gets large, the solver will try many 
  solutions that are symmetrical. So in order to reduce the number 
  of possibile solutions, I need to tell the solver that an order 
  array is symmetrical if it results in the same color sequencing. 
  Or, in other words, that colors are interchangeable.

  My question is how to construct such a constraint?
  """

  Hmm, for some reason, the user Simon deleted the question after
  I answered it. Why?

  My answer was:
  """
  One idea to do symmetry breaking is the following (see model below): 
  for equal values in the optimal list enforce that the permutation index 
  of the first value is less than the permutation index for the second 
  value.

  I've also included the predicate permutation3 which calculates 
  the permutation given the a "source" array and a "sink" array.

  What I understand from your description, optimal is given from some 
  other part of the model so it's shown as fixed here.


  <the MiniZinc model> 

  Instead of 8 solutions, it outputs the unique solution:

  colors: [3, 5, 5, 2, 1, 3, 1]
  perm: [5, 7, 1, 6, 4, 2, 3]
  optimal: [1, 1, 3, 3, 2, 5, 5]
  ----------
  ==========

  """

  Without symmetry breaking there are 8 solutions.
  Here are the permutations for the different solutions:

    perm   :  [6, 4, 0, 5, 3, 1, 2]
    perm   :  [6, 4, 5, 0, 3, 1, 2]
    perm   :  [6, 4, 0, 5, 3, 2, 1]
    perm   :  [6, 4, 5, 0, 3, 2, 1]
    perm   :  [4, 6, 0, 5, 3, 1, 2]
    perm   :  [4, 6, 0, 5, 3, 2, 1]
    perm   :  [4, 6, 5, 0, 3, 1, 2]
    perm   :  [4, 6, 5, 0, 3, 2, 1]

  For example, in perm[0] and perm[1] there are the values
  6 and 4 which corresponds to the positions of the two 1s 
  in the colors array. There are 4 such duplicates in
  the colors array (and the optimal array) : 1, 2, 3, and 5,
  thus there are 
    2 * 4 == 8 
  different solutions without symmetry breaking.

  This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my MiniZinc page: http://www.hakank.org/minizinc

*/

include "globals.mzn"; 
int: n = 7;
array[1..n] of int: colors = [3, 5, 5, 2, 1, 3, 1];
array[1..n] of var 1..n: perm;
array[1..n] of var min(colors)..max(colors): optimal;

%
% permutation3
%
% Note: If permutation is not fixed and from (and to) has more than one
%       occurrence of some value then there may be many solutions.
%
predicate permutation3(array[int] of var int: from,
                           array[int] of var int: perm,
                           array[int] of var int: to) =
    all_different(perm) /\
    forall(i in index_set(perm)) (
       to[i] = from[perm[i]]
    )
;

solve satisfy;

constraint
    % optimal = [1, 1, 3, 3, 2, 5, 5] /\
    permutation3(colors, perm, optimal)
    % /\
    % % symmetry 
    % forall(i, j in 1..n where i < j) (
    %    if optimal[i] = optimal[j] then
    %      perm[i] < perm[j]
    %    else
    %      true
    %    endif
    %  )
;

output [
  "colors: " ++ show(colors) ++ "\n" ++
  "perm: " ++ show(perm) ++ "\n" ++
  "optimal: " ++ show(optimal) ++ "\n"
];
