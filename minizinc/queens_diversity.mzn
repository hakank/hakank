% 
% n-queens problem with degree of diversity of a set of solutions in MiniZinc.
% 
% The objective is to diversify 9 solutions of 10-queens problem, using two
% extra constraint lex_chain_less and soft_alldifferent (defined in this model).
%
% Requires MiniZinc 2.0.
%
% From 
% http://www.emn.fr/z-info/sdemasse/gccat/Kdegree_of_diversity_of_a_set_of_solutions.html
% """
% S1=[0,2,5,7,9,4,8,1,3,6],
% S2=[0,3,5,8,2,9,7,1,4,6],
% S3=[1,3,7,2,8,5,9,0,6,4],
% S4=[2,4,8,3,9,6,1,5,7,0],
% S5=[3,6,9,1,4,7,0,2,5,8],
% S6=[5,9,2,6,3,1,8,4,0,7],
% S7=[6,8,1,5,0,2,4,7,9,3],
% S8=[8,1,4,9,7,0,3,6,2,5],
% S9=[9,5,0,4,1,8,6,3,7,2]
% 
% The costs associated with the soft_alldifferent_ctr constraints of columns 
% 1,2,...,10 are respectively equal to 
% 1, 1, 1, 0, 1, 0, 1, 1, 1, and 1. 
% """

%
% Here's is a better - and proven optimal - solution with just 
% one not distinct column.
%
%   soft_cost: [0, 0, 0, 0, 0, 0, 1, 0, 0, 0]
%   total_cost: 1
%   [ 1,  3,  6,  9,  7, 10,  4,  2,  8,  5]
%   [ 3, 10,  8,  2,  4,  9,  7,  5,  1,  6]
%   [ 4,  2,  9,  6, 10,  7,  1,  3,  5,  8]
%   [ 5,  9,  2,  4,  8,  1,  3,  6, 10,  7]
%   [ 6,  4,  7, 10,  3,  5,  8,  1,  9,  2]
%   [ 7,  5, 10,  1,  6,  4,  2,  8,  3,  9]
%   [ 8,  1,  4,  7,  9,  2,  5, 10,  6,  3]
%   [ 9,  6,  3,  5,  2,  8, 10,  7,  4,  1]
%   [10,  8,  5,  3,  1,  6,  2,  9,  7,  4]
%

% 
% For n = 8, the minimal total_cost is 3:
%
% soft_cost: [0, 0, 0, 1, 0, 1, 0, 1]
% total_cost: 3
% [1, 7, 4, 6, 8, 2, 5, 3]
% [2, 8, 6, 1, 3, 5, 7, 4]
% [3, 5, 7, 1, 4, 2, 8, 6]
% [4, 6, 1, 5, 2, 8, 3, 7]
% [5, 3, 8, 4, 7, 1, 6, 2]
% [7, 1, 3, 8, 6, 4, 2, 5]
% [8, 2, 5, 3, 1, 7, 4, 6]
%

% For n=7 the total_cost = 0
% soft_cost: [0, 0, 0, 0, 0, 0, 0]
% total_cost: 0
% [1, 3, 5, 7, 2, 4, 6]
% [2, 4, 6, 1, 3, 5, 7]
% [3, 5, 7, 2, 4, 6, 1]
% [4, 6, 1, 3, 5, 7, 2]
% [5, 7, 2, 4, 6, 1, 3]
% [6, 1, 3, 5, 7, 2, 4]
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 10;

% decision variables
array[1..n-1, 1..n] of var 1..n: x;

% costs for soft_all_different
% array[1..n] of var 0..(n-1)*n div 2: soft_cost;
array[1..n] of var 0..1: soft_cost;

% var 0..n*((n-1)*n div 2): total_cost = sum(soft_cost);
var 0..n: total_cost = sum(soft_cost);


predicate queens(array[int] of var int: q) =
   all_different(q) /\
   all_different([q[i]+i | i in 1..n]) /\
   all_different([q[i]-i | i in 1..n])
;

%
% Require that all the rows are lexicographically sorted
% (but not the columns as in lex2).
% See: http://www.emn.fr/z-info/sdemasse/gccat/Clex_chain_less.html
%
predicate lex_chain_less(array[int, int] of var int: a) =
  forall(i in 2..card(index_set_1of2(a))) (
     (lex_less( [a[i-1, j] | j in index_set_2of2(a)], 
                [a[i  , j] | j in index_set_1of2(a)]))
  )
;

%
% b is the number of pairs that have the same value.
%
% See http://www.emn.fr/z-info/sdemasse/gccat/Csoft_alldifferent_ctr.html
%
function var int: soft_alldifferent(array[int] of var int: a) =
    sum(i,j in index_set(a) where i < j) (a[i] = a[j])
;


% solve minimize total_cost;
% solve satisfy;
solve :: int_search(soft_cost ++ array1d(x), first_fail, indomain_min, complete) minimize total_cost;
% solve :: int_search(c ++ array1d(x), input_order, indomain_min, complete) satisfy;

constraint
  forall(i in 1..n-1) (
    queens([x[i,j] | j in 1..n])
  )
  /\
  forall(j in 1..n) (
    % soft all different on columns
    soft_cost[j] = soft_alldifferent([x[i,j] | i in 1..n-1])
  )
  /\
  lex_chain_less(x)

  % symmetry breaking
  % /\ x[1,1] = 1

  % /\ % Checking (the soft costs from the example shown above)
  % if n = 10 then 
  %    soft_cost = [1,1,1,0,1,0,1,1,1,1]
  % else 
  %    true
  % endif
  

;

output 
[
  "soft_cost: \(soft_cost)\n",
  "total_cost: \(total_cost)\n"
]
++
[  
  show([x[i,j] | j in 1..n]) ++ "\n"
  | i in 1..n-1
];

