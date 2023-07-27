% 
% Global constraint sort_permutation in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Csort_permutation.html
% """
% sort_permutation(FROM,PERMUTATION,TO)
% 
% Purpose
%
%     The variables of collection FROM correspond to the variables of collection TO according 
%     to the permutation PERMUTATION. The variables of collection TO are also sorted in 
%     increasing order.

% Example
%     (
%     <1, 9, 1, 5, 2, 1> ,
%     <1, 6, 3, 5, 4, 2>,
%     <1, 1, 1, 2, 5, 9>
%     )
%
% The sort_permutation constraint holds since:
%   * The first item FROM[1].var=1 of collection FROM corresponds to the 
%     PERMUTATION[1].var=1th item of collection TO.
%   *  The second item FROM[2].var=9 of collection FROM corresponds to the 
%      PERMUTATION[2].var=6th item of collection TO.
%   * The third item FROM[3].var=1 of collection FROM corresponds to the 
%     PERMUTATION[3].var=3th item of collection TO.
%   * The fourth item FROM[4].var=5 of collection FROM corresponds to the 
%     PERMUTATION[4].var=5th item of collection TO.
%   * The fifth item FROM[5].var=2 of collection FROM corresponds to the 
%     PERMUTATION[5].var=4th item of collection TO.
%   *  The sixth item FROM[6].var=1 of collection FROM corresponds to the 
%      PERMUTATION[6].var=2th item of collection TO.
%   *  The items of collection TO=1,1,1,2,5,9 are sorted in increasing order.
%
% Figure 4.270.1. Illustration of the correspondence between the items of the FROM 
% and the TO collections according to the permutation defined by the items of 
% the PERMUTATION collection
% """


% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 6;
array[1..n] of var 1..9: from;
array[1..n] of var 1..n: permutation;
array[1..n] of var 1..9: to;

%
% sort_permutation
% Note: If permutation is not fixed and from (and to) has more than one occurrence of 
% some value then there may be many solutions.
%
predicate sort_permutation(array[int] of var int: from,
                           array[int] of var int: permutation,
                           array[int] of var int: to) =

    all_different(permutation)
    /\
    increasing(to) % the sort part
    /\ % the partition part
    forall(i in index_set(permutation)) (
       from[i] = to[permutation[i]]
       /\
       to[i] = from[permutation[i]]
    )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

solve satisfy;

constraint

    cp1d(from, [1, 9, 1, 5, 2, 1])
    % /\ cp1d(permutation, [1, 6, 3, 5, 4, 2])
    /\
    cp1d(to, [1, 1, 1, 2, 5, 9])
    /\
    sort_permutation(from, permutation, to)

;

output [
  "from: " ++ show(from) ++ "\n" ++
  "permutation: " ++ show(permutation) ++ "\n" ++
  "to: " ++ show(to) ++ "\n"
];
