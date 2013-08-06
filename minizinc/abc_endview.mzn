% 
% ABC End View puzzle in MiniZinc.
% 
% This is also knows as "Easy as ABC" and "Last Man Standing".
% 
% From Fun With Puzzles: "ABC End View: A1"
% http://www.funwithpuzzles.com/2009/12/abcd-end-view-a1.html
% """
% This the classical puzzle and appeared in many World Puzzle Championships.
% 
% This puzzle is also known with following names
% 1. Easy as ABC
% 
% "ABC End View" Puzzle Instructions:
% Enter the letters ABC such that each letter is exactly once, in all of the 
% rows and columns. One cells will remain empty in each row and column. 
% The letters outside the grid show which letter you come across first 
% from that direction.
% 
%       ABC End View
% 
%            A               
%        _ _ _ _
%      C _ _ _ _
%      C _ _ _ _ B
%        _ _ _ _
%          B
% """
%
% Note: There are some problem instances below that use A..D and
%       5x5 or 6x6 grid which means that there may be 2 empty cells
%       before/after.
%
%
% Also see:
%   http://www.janko.at/Raetsel/AbcEndView/index.htm
%
% The diagonal constraint means that the two diagonal should
% also be alldifferent (except empty cells). See below 
% for an example (problem 168) from 
% http://www.janko.at/Raetsel/AbcEndView/168.a.htm
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n;
int: max_letter;
int: dist = n-max_letter+1; % number of accepted empty cells before/after
bool: diagonal; % has diagonal constraint?

% hints
array[1..n] of int: row_upper;
array[1..n] of int: row_lower;

array[1..n] of int: col_left;
array[1..n] of int: col_right;

% For problem instance
int: A = 1;
int: B = 2;
int: C = 3;
int: D = 4;
int: E = 5;
int: F = 6;

% Presentation
array[0..6] of string: str = array1d(0..6, ["_", "A","B","C","D","E","F"]);

% For global cardinality
array[0..max_letter] of int: gcc = array1d(0..max_letter, [i | i in 0..max_letter] );
array[0..max_letter] of int: counts = array1d(0..max_letter, [n-max_letter] ++ [1 | i in 1..max_letter] );


% decision variables
array[1..n, 1..n] of var 0..max_letter: x;

solve satisfy;

constraint
  trace("gcc: " ++ show(gcc) ++ "\n" ++
        "counts: " ++ show(counts) ++ "\n" ++
        "dist: " ++ show(dist) ++ "\n", 1=1)
;

%
% a start with letter c and accept d empty slots.
% 
predicate start_with(array[int] of var int: a, int: c, int: d) =
   if c > 0 then
     exists(i in 1..d) (
       a[i] = c 
       /\       
       forall(j in 1..i-1) (
          a[j] = 0
       )
     )
   else 
     true
   endif
;


%
% a ends with letter c and accept d empty slots
%
predicate end_with(array[int] of var int: a, int: c, int: d) =
   let {
     int: len = length(a)
   } in
   if c > 0 then
      exists(i in n-d..n) (
        a[i] = c 
        /\       
        forall(j in i+1..n) (
          a[j] = 0
        )
     )
   else
      true
   endif
;


%
% Latin square except 0
%
constraint
   forall(i in 1..n) (
      global_cardinality([x[i,j] | j in 1..n], gcc, counts) 
   )
   /\
   forall(j in 1..n) (
      global_cardinality([x[i,j] | i in 1..n], gcc, counts) 
   )

   /\
   if diagonal then 
     global_cardinality([x[i,i] | i in 1..n], gcc, counts) 
     /\
     global_cardinality([x[i,n-i+1] | i in 1..n], gcc, counts) 
   else 
     true
   endif
;

%
% The hints
%
constraint
  forall(j in 1..n) (
     let {
       array[1..n] of var 0..n-1: tmp = [x[i,j] | i in 1..n]
     } in
     start_with(tmp, row_upper[j], dist)
     /\
     end_with(tmp, row_lower[j], dist)
  )
  /\
  forall(i in 1..n) (
     let {
       array[1..n] of var 0..n-1: tmp = [x[i,j] | j in 1..n]
     } in
     start_with(tmp, col_left[i], dist)
     /\
     end_with(tmp, col_right[i], dist)
  ) 
;


output 
[
   if j = 1 then "\n" else " "  endif ++
     show(str[fix(x[i,j])])
   | i, j in 1..n
]
++ ["\n"]
;

%
% data
% 


% Problem instance from
% http://www.funwithpuzzles.com/2009/12/abcd-end-view-a1.html
% This is a 4x4 with 1 blank
% n = 4;
% max_letter = C;
% row_upper = [0,0,A,0];
% row_lower = [0,B,0,0];
% col_left  = [0,C,C,0];
% col_right = [0,0,B,0];
% diagonal = false;


% From Dan Moore: Brainpower Bible
% (introduction example)
% Note: This is a 5x5 grid with 2 blanks
% n = 5;
% max_letter = C;
% row_upper = [C,C,A,C,0];
% row_lower = [B,B,0,A,0];
% col_left  = [0,A,0,0,0];
% col_right = [0,B,A,B,0];
% diagonal = false;


% From Dan Moore: Brainpower Bible
% Alfa 1 (5x5)
% n = 5;
% max_letter = C;
% row_upper = [0,B,B,C,0];
% row_lower = [B,C,A,A,B];
% col_left  = [0,0,B,B,C];
% col_right = [0,B,0,A,B];
% diagonal = false;

% From Dan Moore: Brainpower Bible
% Alfa 2 (5x5)
% n = 5;
% max_letter = C;
% row_upper = [B,0,0,C,B];
% row_lower = [0,A,B,A,C];
% col_left  = [0,0,0,A,B];
% col_right = [A,B,0,0,0];
% diagonal = false;

% % From Dan Moore: Brainpower Bible
% % Delta 1 (6x6)
% n = 6;
% max_letter = D;
% row_upper = [D,D,A,0,C,A];
% row_lower = [0,0,0,C,0,0];
% col_left  = [D,0,D,0,0,A];
% col_right = [0,0,A,D,C,C];
% diagonal = false;

% From Dan Moore: Brainpower Bible
% Delta 2 (6x6)
% n = 6;
% max_letter = D;
% row_upper = [0,B,D,D,0,C];
% row_lower = [B,0,0,B,C,D];
% col_left  = [A,D,C,0,B,D];
% col_right = [0,0,B,B,0,0];
% diagonal = false;


% From http://www.cross-plus-a.com/puzzles.htm#EasyAsABC
% (6x6)
% n = 6;
% max_letter = E;
% row_upper = [A,0,0,E,0,0];
% row_lower = [0,B,C,0,0,A];
% col_left  = [0,0,A,D,0,0];
% col_right = [E,0,B,E,0,0];
% diagonal = false;


% From http://www.janko.at/Raetsel/AbcEndView/209.a.htm
% ABC End View Nr. 209
% (7x7)
% (Difficulty 8, "schwer")
% n = 7;
% max_letter = E;
% row_upper = [A,0,B,0,0,C,B];
% row_lower = [0,A,0,B,B,D,0];
% col_left  = [A,C,A,0,E,0,E];
% col_right = [0,D,0,C,0,E,0];
% diagonal = false;

% From http://www.janko.at/Raetsel/AbcEndView/046.a.htm
% ABC End View Nr. 46
% (7x7)
% (Difficulty 8, "schwer")
% n = 7;
% max_letter = E;
% row_upper = [0,0,D,C,0,0,C];
% row_lower = [0,0,E,D,D,C,0];
% col_left  = [0,E,C,C,E,B,0];
% col_right = [0,0,B,0,0,C,0];
% diagonal = false;


% http://www.janko.at/Raetsel/AbcEndView/168.a.htm
% ABC End View Nr. 168
% (7x7)
% (Difficulty 8, "schwer")
% NOTE: This has the diagonal requirement.
n = 7;
max_letter = F;
row_upper = [A,0,C,E,F,A,0];
row_lower = [0,D,0,C,0,0,E];
col_left  = [A,0,0,C,E,D,0];
col_right = [0,C,B,0,0,E,0];
diagonal = true;
