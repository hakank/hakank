% 
% Sudoku using global cardinality in MiniZinc.
%
% The standard way of solving Sudoku is using the alldifferent constraint
% on rows, columns, and regions.
% 
% Here we instead use of constraint global_cardinality:
% http://www.emn.fr/x-info/sdemasse/gccat/Cglobal_cardinality.html
% 
% 
% For a discussion about this approach see my blog post:
% "Solving Pi Day Sudoku 2009 with the global cardinality constraint"
% http://www.hakank.org/constraint_programming_blog/2009/03/solving_pi_day_sudoku_2009_wit.html
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


% The problem is from
%  http://www.tellmehowto.net/sudoku/veryhardsudoku.html
%
%
% First: using only the all_different constraint (full search tree) with
% Geocode/FlatZinc:
% 
%         runtime:       0
%         solutions:     1
%         propagators:   27
%         propagations:  167
%         nodes:         17
%         failures:      8
%         peak depth:    4
%         peak memory:   26 KB
%
%
% Using both all_different and global_cardinality, using MiniZinc's standard
% decomposition of global_cardinality:
%
%         runtime:       0
%         solutions:     1
%         propagators:   0
%         propagations:  6848
%         nodes:         1
%         failures:      0
%         peak depth:    0
%         peak memory:   326 KB
%
%
% Just global cardinality:
%         runtime:       0
%         solutions:     1
%         propagators:   0
%         propagations:  7967
%         nodes:         1
%         failures:      0
%         peak depth:    0
%         peak memory:   326 KB
%



include "globals.mzn"; 

int: n = 9;
array[1..n,1..n] of var 1..n: x;
% the number of occurrences of each number in a section
array[1..n] of 1..n: occ = [1,1,1,1,1,1,1,1,1];

predicate cp2d(array[int,int] of var int: x, array[int,int] of var int: y) =
  assert(index_set_1of2(x) = index_set_1of2(y) /\
         index_set_2of2(x) = index_set_2of2(y),
           "cp2d: x and y have different sizes",
     forall(i in index_set_1of2(x), j in index_set_2of2(x)) (
         y[i,j] = x[i,j]
    ) 
  )
; 

solve satisfy;
% solve :: int_search([x[i,j] | i,j in 1..n], "first_fail", "indomain_min", "complete") satisfy;
% solve :: labelling_ff satisfy;

% All different in rows and all different in columns.
%
constraint 
  forall(i in 1..n) (
    % all_different([x[i,j] | j in 1..n]) /\
    global_cardinality([x[i,j] | j in 1..n], set2array(index_set(occ)), occ)
  )
;

constraint
  forall(j in 1..n) (
    % all_different([x[i,j] | i in 1..n]) /\
    global_cardinality([x[i,j] | i in 1..n], set2array(index_set(occ)), occ)
  ) 
;

constraint
  forall(i in 0..2,j in 0..2) (
    % all_different([x[r,c] | r in i*3+1..i*3+3, c in j*3+1..j*3+3] ) /\
    global_cardinality([x[r,c] | r in i*3+1..i*3+3, c in j*3+1..j*3+3], 
        set2array(index_set(occ)),
        occ)
  )

;

constraint

% The first two is from MiniZinc's distributed model sudoku.mzn
%       cp2d(x,[|
%       _, _, _, _, _, _, _, _, _,
%       |_, 6, 8, 4, _, 1, _, 7, _,
%       |_, _, _, _, 8, 5, _, 3, _,
%       |_, 2, 6, 8, _, 9, _, 4, _,
%       |_, _, 7, _, _, _, 9, _, _,
%       |_, 5, _, 1, _, 6, 3, 2, _,
%       |_, 4, _, 6, 1, _, _, _, _,
%       |_, 3, _, 2, _, 7, 6, 9, _,
%       |_, _, _, _, _, _, _, _, _
%       |])
%
%
%       % http://www.tellmehowto.net/sudoku/veryhardsudoku.html
%       cp2d(x, [| 
%          _,_,6, _,_,_, _,9,_,
%         |_,_,_, 5,_,1, 7,_,_,
%         |2,_,_, 9,_,_, 3,_,_,
      
%         |_,7,_, _,3,_, _,5,_,
%         |_,2,_, _,9,_, _,6,_,
%         |_,4,_, _,8,_, _,2,_,

%         |_,_,1, _,_,3, _,_,4,
%         |_,_,5, 2,_,7, _,_,_,
%         |_,3,_, _,_,_, 8,_,_|])


    % From Gecode sudoku.cpp
    % """
    % 5
    % Hard one from http://www.cs.mu.oz.au/671/proj3/node5.html
    % """
    cp2d(x, array2d(1..n, 1..n, [
    _,_,_,_,_,3,_,6,_,
    _,_,_,_,_,_,_,1,_,
    _,9,7,5,_,_,_,8,_,
    _,_,_,_,9,_,2,_,_,
    _,_,8,_,7,_,4,_,_,
    _,_,3,_,6,_,_,_,_,
    _,1,_,_,_,2,8,9,_,
    _,4,_,_,_,_,_,_,_,
    _,5,_,1,_,_,_,_,_
    ]))


    % From http://www.koalog.com/resources/samples/SudokuProblem.java.html
    % """
    % A very very hard instance generated for http://sudoku.koalog.com.
    % """
%     cp2d(x, array2d(1..n, 1..n, [
%         4,_,_,_,3,9,_,2,_,
%         _,5,6,_,_,_,_,_,_,
%         _,_,_,_,_,_,6,_,4,
%         _,_,_,_,_,_,9,_,_,
%         5,_,_,1,_,_,2,_,_,
%         _,9,_,_,2,7,_,3,_,
%         _,3,7,_,_,_,_,_,_,
%         _,_,_,_,_,_,8,_,6,
%         9,_,8,_,1,_,_,_,_
%     ]))
;


output [
  if j = 1 then "\n" else " " endif ++
  show(x[i,j])
  | i,j in 1..n
] ++ ["\n"];
