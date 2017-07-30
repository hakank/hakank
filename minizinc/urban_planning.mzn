% 
% Urban Planning (PuzzleOR) in MiniZinc.
% 
% 
% http://puzzlor.editme.com/urbanplanning
% """
% Urban planning requires careful placement and distribution of commercial 
% and residential lots.  Too many commercial lots in one area leave no room 
% for residential shoppers.  Conversely, too many residential lots in one 
% area leave no room for shops or restaurants.
% 
% The 5x5 grid in Figure 1 shows a sample configuration of residential and 
% commercial lots.  Your job is to reorder the 12 Residential green lots 
% and 13 Commercial red lots to maximize the quality of the layout.  The 
% quality of the layout is determined by a points system.  Points are awarded as follows:
%     Any column or row that has 5 Residential lots = +5 points
%     Any column or row that has 4 Residential lots = +4 points
%     Any column or row that has 3 Residential lots = +3 points
%     Any column or row that has 5 Commercial lots = -5 points
%     Any column or row that has 4 Commercial lots = -4 points
%     Any column or row that has 3 Commercial lots = -3 points
% 
% For example, the layout displayed in Figure 1 has a total of 9 points:
% Points for each column, from left to right = -3, -5, +3, +4, +3
% Points for each row, from top to bottom = +3, +3, +3, +3, -5
%
% Question:  What is the maximum number of points you can achieve for the layout?
% """
% 
% This model yields 600 optimal solutions.
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
int: n = 5;

int: residential = 1;
int: commercial = 2;

% scores
% 5 Residential lots = +5 points
% 4 Residential lots = +4 points
% 3 Residential lots = +3 points
% 5 Commercial lots = -5 points
% 4 Commercial lots = -4 points
% 3 Commercial lots = -3 points

% score table (for residental)
% Note: it's from 0..5.
array[0..5] of int: score_table =
   array1d(0..5, [
      -5, -4,-3, 3, 4, 5, % for residential
    ]);

% decision variables
array[1..n, 1..n] of var 1..2: x;

array[1..n] of var -5..5: row_scores;
array[1..n] of var -5..5: col_scores;

var 0..50: z = sum(row_scores) + sum(col_scores);


% scores for this row/column. Note we just count the
% residential score
predicate scores(array[int] of var int: rc, var int: score) =
   let {
     var 0..n: cc
   } in 
   count(rc,1, cc) 
   /\
   score = score_table[cc]
;

% slower
predicate scores2(array[int] of var int: rc, var int: score) =
   score = score_table[sum([bool2int(rc[i]=1) | i in 1..n ])]
;


% solve maximize z;
solve :: int_search(
   [x[i,j] | i,j in 1..n] ++
   row_scores ++ col_scores, % ++ [x[i,j] | i,j in 1..n], 
   first_fail, 
   indomain_min, 
   complete) 
  % satisfy;
  maximize z;

constraint

   % z = 14 /\ % for solve satisfy

   forall(i in 1..n) (
      scores([x[i,j] | j in 1..n], row_scores[i]) /\
      scores([x[j,i] | j in 1..n], col_scores[i])
   )
   /\
   % 12 residential
   count([x[i,j] | i,j in 1..n], residential,12) 
 
   % symmetry breaking
   /\ x[1,1] = residential
;

output [
  "z: " ++ show(z) ++ "\n" ++
  "row_scores: " ++ show(row_scores) ++ "\n" ++
  "col_scores: " ++ show(col_scores) ++ "\n" ++ 
  "x:"
] ++
[
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i,j in 1..n
] ++
["\n"]++
[
  if j = 1 then "\n" else " " endif ++
    if fix(x[i,j]) == residential then 
     "R"
    else 
     "C"
    endif
  | i,j in 1..n
]

;

