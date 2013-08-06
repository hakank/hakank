% 
% Nonogram solver in MiniZinc.
%
% This version of nonogram solver use the regular constraint
% as well as a "hand-made" automaton.
% 
% For more about this, see my blog post:
% "At last, a Nonogram solver using regular constraint in MiniZinc"
% http://www.hakank.org/constraint_programming_blog/2009/09/at_last_a_nonogram_solver_usin_1.html
% 
% See also:
%  MiniZinc: http://www.hakank.org/minizinc/nonogram.mzn (slow version)
%  Comet: http://www.hakank.org/comet/nonogram_regular.co
%  Comet: http://www.hakank.org/comet/nonogram_automaton.co
% 
%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

int: row_max;
int: row_num_patterns;
int: row_max_state;
int: row_total_states;
array[1..row_total_states, 1..2] of 0..row_max_state: row_states; 
array[1..row_num_patterns] of 0..row_max_state: row_num_states;
array[1..row_num_patterns] of 0..row_total_states: row_start_where;

int: col_max;
int: col_num_patterns;
int: col_max_state;
int: col_total_states;
array[1..col_total_states, 1..2] of 0..col_max_state: col_states; 
array[1..col_num_patterns] of 0..col_max_state: col_num_states;
array[1..col_num_patterns] of 0..col_total_states: col_start_where;

%
% decision variable
%
array[1..row_max, 1..col_max] of var 1..2: x; % Note, must be 1..2.


%
% Note: This is a "reversed" labeling where we label the columns (j) before
%       the rows (i).
solve :: int_search(
        [x[i,j] | j in 1..col_max, i in 1..row_max],
        first_fail, % occurrence,
        indomain_min,
        complete
        ) 
    satisfy;

% solve satisfy;

predicate make_rules(array[int] of var int: x,
                      int: num_states,
                      array[int,int] of int: states) =
        regular(
             x,
             num_states, 
             2, 
             states,
             1, 
             {num_states})
;

constraint

     forall(i in 1..row_num_patterns) (
         let {
             int: kto   = row_start_where[i]+row_num_states[i]-1
         } in
         make_rules(
             [x[i,j] | j in 1..col_max],
             row_num_states[i], 
             array2d(1..row_num_states[i], 1..2, [row_states[k,j] | k in row_start_where[i]..kto, j in 1..2]))
     )

     /\

     forall(i in 1..col_num_patterns) (
        let {
            int: kto   = col_start_where[i]+col_num_states[i]-1
        } in
        make_rules(
             [x[j,i] | j in 1..row_max],
             col_num_states[i], 
             array2d(1..col_num_states[i], 1..2, [col_states[k,j] | k in col_start_where[i]..kto, j in 1..2]))
     )

;

%
% This works (for now at least) only with the minizinc solver.
%
output 
[
  if j = 1 then "\n" else "" endif ++
     show_cond(x[i,j] = 1, " ", "#") 
    
  | i in 1..row_max, j in 1..col_max
] 
++ 
[
  "\n"
];
