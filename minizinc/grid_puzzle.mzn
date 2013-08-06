% 
% Grid puzzle in MiniZinc.
% 
% From StackOverflow:
% Riddle in Prolog
% http://stackoverflow.com/questions/9955366/riddle-in-prolog
% """
% I've got a problem.
% I must solve a riddle in Prolog, but I haven't got any idea how to do this.
% Maybe can you help me, give me some hint or something.
% First I haven't real name of this riddle, it's calld simply "ABC".
% At the beggining they give me size of board (n x m) and n, m = (1,10) 
% and one letter, for example C, so I can use in ma solution only letters form A to C.
% Then I get some threes in form (i,j,k) , i <= n, j<=m and k belong to (A,C). 
% For example board it the beggining look like this:
%
%   _ _ B A
%   B _ _ C
%   C _ _ _
%   _ A _ _
% 
% To every empty boxe I must type the letters A,B,C in such a 
% way, that when i finish typing they must meet the following conditions:
%
%    if in a row (column) are different letters, every of letter is appearing in this row (column) same number of times
%    at least in one row (column) all letters are the same.
%
% Have you any idea how to solve this kind of riddle?
% Maybe this puzzle has its own name and I can read somewhere about it?
% I'll be very grateful for every help!
%
% ...
% 
% Little correct: In only one row OR column all letters are the same, not in both. 
% Solution for my example is:
%
% B A B A
% B C B C
% C C C C
% C A C A
% """

% In numeric representation
%
% 2 1 2 1
% 2 3 2 3
% 3 3 3 3
% 3 1 3 1
% """


% Note: Without any hints at all, there are 864 different
% solutions with n=4 and m=3 (A,B,C)
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
int: n = 4;
int: m = 3; % A..C

int: A = 1;
int: B = 2;
int: C = 3;

array[1..m] of string: str = ["A","B","C"];

array[1..n, 1..n] of int: grid;
array[1..m] of int: s = [i | i in 1..m];

% decision variables
array[1..n, 1..n] of var 1..m: x;


% Note: I make these global for debuggin purposes
array[1..n, 1..m] of var 0..n: gcc_rows;
array[1..n, 1..m] of var 0..n: gcc_cols;


% solve satisfy;
solve :: int_search(
   [gcc_rows[i,j] | i in 1..n , j in 1..m] ++ [gcc_cols[i,j] | i in 1..n, j in 1..m], 
   first_fail, 
   indomain_min, 
   complete) 
satisfy;


% all values > 0 must be the same
predicate same_except_0(array[int] of var int: v) =
   forall(i,j in index_set(v) where i < j) (
       (v[i] > 0 /\ v[j] > 0) -> (v[i] = v[j])
   )
;

% all values must be the same
predicate same(array[int] of var int: v) =
   forall(i,j in index_set(v) where i < j) (
       v[i] = v[j]
   )
;



% hints
constraint
  forall(i,j in 1..n) (
    if  grid[i,j] > 0 then
      x[i,j] = grid[i,j]
    else
      true
    endif
  )
;


constraint
  forall(i in 1..n) (
      global_cardinality([x[i,j] | j in 1..n], s, [gcc_rows[i,k]| k in 1..m])
      /\
      same_except_0([gcc_rows[i,k] | k in 1..m])
   )
   /\
   forall(j in 1..n) (
      % Note: we transpose gcc_cols to be row based
      global_cardinality([x[i,j] | i in 1..n], s, [gcc_cols[j,k]| k in 1..m])
      /\
      same_except_0([gcc_cols[j,k] | k in 1..m])
  )
;


%
% Exact one occurrence where all are the same value in x
%
constraint
   sum(i in 1..n) (
       bool2int( same([x[i,j] | j in 1..n]) ) +
       bool2int( same([x[j,i] | j in 1..n]) )
   ) == 1
;



output [
 "x: "
]
++
[
  if j = 1 then "\n" else " " endif ++
    show(str[fix(x[i,j])])
  | i,j in 1..n
]
++
[ "\ngcc_row:" ]
++
[
  if j = 1 then "\n" else " " endif ++
    show(gcc_rows[i,j])
  | i in 1..n, j in 1..m

]
++
[ "\ngcc_col:" ]
++
[
  if j = 1 then "\n" else " " endif ++
    show(gcc_cols[i,j])
  | i in 1..n, j in 1..m

] 
 ++ ["\n"]
;

%
% data
%
grid = array2d(1..n, 1..n, 
  [
   0, 0, B, A,
   B, 0, 0, C,
   C, 0, 0, 0,
   0, A, 0, 0,
  ]);

