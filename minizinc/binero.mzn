% 
% Binero (Binairo, Takuzu, Binary Puzzle) grid puzzle in MiniZinc.
%
% This grid puzzle is also called Takuzu, Binairo, Binary Puzzle, and 
% Tohu wa Vohu. See
% - http://en.wikipedia.org/wiki/Takuzu
% - http://www.cross-plus-a.com/puzzles.htm#Binairo
%
% 
% From the Scampi example Binero.scala
% """
% Binero is a grid game, similar to Sudoku.
% You get a 2n x 2n grid which must be filled with ones and zeroes.
% Some of the symbols are given. Empty squares are represented with -1.
% - On each line and each column there must be as many zeros as ones.
% - No more than 2 zeros or ones can be placed on a line or a column consecutively.
% - Identical columns or lines are forbidden.
% 
% @author VictorJavAdore
% 
% Input examples :
% - First line : an integer n, the half dimension of the grid
% - Next 2n lines : original state of the grid (a dot means a blank square)
% 	5
% 	0.11.....1
% 	..1....0.0
% 	.0..1.1.1.
% 	.1........
% 	1.0..1....
% 	.0..0...11
% 	...1.....1
% 	0..1.1.0..
% 	..0.......
% 	11....11.0
% """

% Compare with 
%    http://www.hakank.org/minizinc/takuzu.mzn
% for another approach.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n;
array[1..2*n, 1..2*n] of int: data;
int: X = -1;

% decision variables
array[1..2*n, 1..2*n] of var 0..1: x;


% solve satisfy;
solve :: int_search(
        [x[i,j] | i,j in 1..2*n], 
        first_fail,
        indomain_min, 
        complete) 
    satisfy;



%
% Ensure that there is atmost m consecutive entries of the same value
% in a row. This is a simpler, and here faster, variant of 
% the global constraint stretch path.
% 
% Cf http://www.hakank.org/minizinc/max_m_in_row.mzn
% 
predicate max_m_in_row(array[int] of var int: a,
                       int: m) =
   let {
       int: lbb = lb_array(a),
       int: ubb = ub_array(a),
       int: start = min(index_set(a)),
       int: end = max(index_set(a))
   } in
   forall(i in start..end-m) (
      let {
        array[lbb..ubb] of var 0..m: gcc
      } in
      global_cardinality([a[i+k] | k in 0..m], array1d(lbb..ubb, [b|b in lbb..ubb]), gcc) :: domain
  )   
;

%
% Ensure that there are max 2 consecutive values in a row
%
% Using regular (automaton)
% 
%       S2 - 0 -> S3   % 2 0's
%     /  | \    /
%   S1   |   /\
%     \  | /     \
%       S4 - 1 -> S4   % 2 1's
% 
%   State Input 
%         0   1
%   -----------
%   S1    S2  S4
%   S2    S3  S4
%   S3    -   S4  % we don't accept a third 0
%   S4    S2  S5
%   S5    S1  -   % we don't accept a third 1
%
% 
% Note that since regular use 0 as invalid state we must
% add 1 to input.
%
% TODO: This could be generalized to max_m_in_row_regular.
%  
predicate max_2_in_row_regular(array[int] of var int: input) =
   let {
   int: n_states = 5,
   int: input_max = 2,
   int: initial_state = 1,
   set of int: accepting_states = {2,3,4,5},
   array[1..n_states, 1..2] of int: transition_fn = 
       [|2, 4,   % s1
        |3, 4,   % s2
        |0, 4,   % s3
        |2, 5,   % s4
        |2, 0|], % s5
   int: len = length(input),
   array[1..len] of var 1..2: input2 = [input[i]+1 | i in 1..len]
    } in
    regular(input2, n_states, input_max, transition_fn, initial_state,
            accepting_states) % :: domain

;

%
% Global constraint stretch_path:
% Ensure that each stretch (span) should contain lmin..lmax 
% elements of the same value.
% See http://www.hakank.org/minizinc/stretch_path.mzn
%
predicate stretch_path(array[int] of var int: x, 
                       array[int] of var int: val, 
                       array[int] of var int: lmin, 
                       array[int] of var int: lmax) =
     let {
          int: lbx = min(index_set(x)),
          int: ubx = max(index_set(x)),
          int: lbval = min(index_set(val)),
          int: ubval = max(index_set(val)),          
          array[lbx..ubx] of var 0..1: first_pos % first positions of a stretch
     }
     in
     % sanity clause 
     forall(s in lbval..ubval) (
       lmin[s] <= lmax[s]
     )
     /\
     % get the index for first positions of the stretches
     first_pos[lbx] = 1 /\
     forall(i in lbx+1..ubx) (
       first_pos[i] = 1 <-> (x[i] != x[i-1])
     )
     /\ 
     % check all values in x
     forall(i in lbx..ubx) (
        let {
           var lbval..ubval: s_ix  % index of the current value in the stretch
        }
        in
        val[s_ix] = x[i]
        /\
        (
         % ignore if we are inside a stretch
         first_pos[i] = 0
         \/
         (
          % when in first position in a stretch, then
          % check that this stretch has is OK
          first_pos[i] = 1 
          <->
          (
            exists(j in i..ubx) (
              % check the interval
              j-i+1 >= lmin[s_ix] /\ 
              j-i+1 <= lmax[s_ix] 

              /\ % all same values in the stretch 
              forall(k in i+1..j) (
                x[k] = x[i]
                %/\ first_pos[k] = 0
            )
            /\ % the neighbours of the stretch are different
            (i > lbx -> x[i-1] != x[i])
            /\
            (j < ubx -> x[j+1] != x[j])
           )
         )
       )         
      )
    )
;

%
% The arrays a and b are not the same.
% Here we ensure that there is at least one element 
% in a and b that is not the same.
%
predicate not_same2(array[int] of var int: a, 
                   array[int] of var int: b) =
    exists(k in index_set(a)) (
        a[k] != b[k] 
    )
;

% Slower variant of not_same2
predicate not_same3(array[int] of var int: a, 
                   array[int] of var int: b) =
    sum(k in index_set(a)) (
        bool2int(a[k] != b[k])
    ) > 0
;


%
% Fill the hints
%
constraint
   forall(i,j in 1..2*n) (
      if data[i,j] > X then
        x[i,j] = data[i,j] 
      else 
        true
      endif
   )
;

%
% - On each line and each column there must be as many zeros as ones.
% 
constraint
   forall(i in 1..2*n) (
     sum([x[i,j] | j in 1..2*n]) == n /\
     sum([x[j,i] | j in 1..2*n]) == n
   )
;

%
% - No more than 2 zeros or ones can be placed on a line or a column consecutively.
%
constraint
   forall(i in 1..2*n) (
     % max_m_in_row([x[i,j] | j in 1..2*n], 2) /\
     % max_m_in_row([x[j,i] | j in 1..2*n], 2)

     %% same #failures as max_m_in_row, 
     %% though fewer variables and propagations
     max_2_in_row_regular([x[i,j] | j in 1..2*n]) /\
     max_2_in_row_regular([x[j,i] | j in 1..2*n])

     %% using stretch_path instead (slower than max_m_in_a_row)
     % stretch_path([x[i,j] | j in 1..2*n], [0,1], [0,0], [2,2]) /\
     % stretch_path([x[j,i] | j in 1..2*n], [0,1], [0,0], [2,2])

   )
;



%
% - Identical columns or lines are forbidden.
%
constraint
   forall(i, j in 1..2*n where i < j) (
      not_same2([x[i,k] | k in 1..2*n],
                [x[j,k] | k in 1..2*n])
      /\
      not_same2([x[k,i] | k in 1..2*n],
                [x[k,j] | k in 1..2*n])
   )
;


output [
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i, j in 1..n*2
]
 ++ ["\n"]
;


%
% data
%

%
% Problem instance from the Scampi data/binero1.txt
%
% n = 5;
% data = array2d(1..2*n, 1..2*n,
% [
% 0,X,1,1,X,X,X,X,X,1,
% X,X,1,X,X,X,X,0,X,0,
% X,0,X,X,1,X,1,X,1,X,
% X,1,X,X,X,X,X,X,X,X,
% 1,X,0,X,X,1,X,X,X,X,
% X,0,X,X,0,X,X,X,1,1,
% X,X,X,1,X,X,X,X,X,1,
% 0,X,X,1,X,1,X,0,X,X,
% X,X,0,X,X,X,X,X,X,X,
% 1,1,X,X,X,X,1,1,X,0
% ]);

%
% Problem instance from the Scampi data/binero2.txt
%
% n = 5;
% data = array2d(1..2*n, 1..2*n,
% [
% X,0,X,0,X,X,X,1,X,X,
% 1,X,X,X,1,X,X,X,X,X,
% X,0,X,0,X,X,X,X,X,X,
% X,X,1,X,X,1,X,X,0,X,
% X,X,X,0,X,X,X,1,X,X,
% 1,X,X,0,X,0,X,X,X,X,
% X,X,1,X,X,X,X,0,X,0,
% X,X,0,1,X,X,X,X,1,X,
% 1,X,X,X,X,X,0,X,X,X,
% 1,X,0,X,X,X,X,X,X,1
% ]);


%
% Problem from http://www.cross-plus-a.com/puzzles.htm#Binairo
%
n = 5;
data = array2d(1..2*n, 1..2*n,
[
0,X,X,X,X,X,X,X,X,X,
X,X,1,X,X,X,1,1,X,X,
X,1,X,X,X,X,X,X,0,X,
X,X,X,0,X,0,X,X,X,X,
X,X,X,X,X,X,X,X,X,X,
0,X,X,X,X,X,X,1,X,0,
X,1,X,1,X,X,X,X,X,X,
0,X,X,X,0,0,X,1,X,X,
1,X,X,X,X,X,X,X,X,X,
X,1,X,1,1,X,X,X,X,0,
]);
