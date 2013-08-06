%
% Partial Latin square in MiniZinc
% 
% http://mathworld.wolfram.com/PartialLatinSquare.html
% """
% In a normal n×n Latin square, the entries in each row and column are chosen from a 
% "global" set of n objects. Like a Latin square, a partial Latin square has no 
% two rows or columns which contain the same two symbols. However, in a partial 
% Latin square, each cell is assigned one of its own set of n possible "local" 
% (and distinct) symbols, chosen from an overall set of more than three distinct 
% symbols, and these symbols may vary from location to location. 
%
% For example, given the possible symbols {1,2,...,6} which must be arranged as
%
%   (1,2,3)   (1,3,4)  (2,5,6)
%   (2,3,5)   (1,2,3)  (4,5,6)
%   (4,3,6)   (3,5,6)  (2,3,5)
%
% the 3×3 partial Latin square
%
%   1,3,2
%   2,1,5
%   6,5,3
% 
% can be constructed.
% """
%
% This MiniZinc model implements the example above, and gives 1185 solutions.
%
% (Cf the model latin_square.mzn)
%
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

int: n;
int: max_val;
array[1..n, 1..n] of var 1..max_val: x;
array[1..n, 1..n] of set of 1..max_val: s;


%
% x must be a latin square, i.e. all element in each row is
% different, and all element in each column is dfifferent
%
predicate latin_square(array[int,int] of var int: m) =
  let {
     int: n = card(index_set_1of2(m))
  }
  in
  forall(i in 1..n) (
       all_different([ m[i, j] | j in 1..n]) /\
       all_different([ m[j, i] | j in 1..n])
  )
;

% solve satisfy;
solve :: int_search([ x[i, j] | i,j in 1..n], first_fail, indomain_min, complete) satisfy;

constraint
  % the elements in each cell is taken from the element in the cell of s
  forall(i, j in 1..n) (
     x[i,j] in s[i,j]
  )
  /\
  latin_square(x)
;


%
% data
%
n = 3;
max_val = 6;
s = [|{1,2,3},  {1,3,4},  {2,5,6},
     |{2,3,5},  {1,2,3},  {4,5,6},
     |{4,3,6},  {3,5,6},  {2,3,5}|];


% Somewhat later note: It seems that there is slightly other explanation
% of partial latin squares. For example http://www.keele.ac.uk/depts/ma/people/db.html ,
% which suggest that the latin square may contain cells where a specific value must 
% be put. In this model it simply means that the cardinality of that set is 1. 
%
% In MiniZinc there is, however, another way of forcing an element in an array/matrix:
% by explicitly state that element. The cells with variable elements is then 
% represented with '_'. 
% Example: the following will force the cell x[1,1] to 2, and let the other cells
% be variable.
%
% x = 
% [
%   2, _, _,
%   _, _, _,
%   _, _, _
% ];


output [
  if j = 1 then "\n" else " " endif ++
  show(x[i,j])
  | i,j in 1..n
] ++ ["\n"];
