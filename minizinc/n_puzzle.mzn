%
% 15-puzzle (n-puzzle) in Minizinc
%
%
% The solution (moves) is in the second column of array moves.
% The first column is just helper.
% 

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn";

% int: t = 9;
int: t = 16;
% the puzzle configuration
array[1..t] of 1..t: puzzle;

% length of the solution
% int: num_sols = 8; 
int: num_sols;

% the moves, position of the empty space (as t)
array[1..num_sols, 1..2] of var 0..t: moves;

array[1..num_sols, 1..t] of var 1..t: x;

% the operations: 0: same, 1: move
array[1..num_sols] of var 0..1: operations;


% valid moves (symmetric matrix)
%     from  to
array[1..t, 1..t] of 0..1: valid_moves;

% is this row the solution?
array[1..num_sols] of var 0..1: check;

% index of first solution
var 1..num_sols: check_ix;

% the changes
array[1..num_sols] of var 0..t: changes;

% 
% predicate same: array y is the same as array x
%
predicate same(array[int] of var int: x, array[int] of var int: y) =
   let { 
     int: n = card(index_set(x))
   }
   in
   forall(i in 1..n) (
      y[i] = x[i]
   )
;


% solve satisfy;
% solve minimize check_ix;
solve :: int_search(
        [x[i,j] | i in 1..num_sols, j in 1..t]
        ++ [moves[i,j] | i in 1..num_sols, j in 1..2] 
        ++ operations
        ++ [valid_moves[i,j] | i, j in 1..t]
        ,
        largest, % first_fail, 
        indomain_split, 
        complete) 
minimize check_ix;

constraint

    % initializations
    check[1] = 0
    /\
    moves[1, 1] = 0
    /\
    changes[1] = 0
    /\

    % identify the empty block in the puzzle configuration
    exists(j in 1..t) (
       x[1, j] = t
       /\
       moves[1, 2] = j
    )
    /\ % initialize first row in x with the puzzle
    forall(j in 1..t) (
      x[1,j] = puzzle[j]
    )
    /\ % last row should be 1..t (this is the goal)
    forall(j in 1..t) (
       x[num_sols, j] = j
    )
    /\ % select the operations
   forall(i in 2..num_sols) (

     all_different([x[i, j] | j in 1..t]) % :: domain
     /\
     % at most 2 changes of each move
     changes[i] = sum(j in 1..t) (bool2int(x[i,j] != x[i-1,j]))
     /\
     (changes[i] = 2 \/ changes[i] = 0)
     /\
     ( % is this row a solution (i.e. 1..t)? Then we're done.
       check[i] = 1 
       <-> 
       same([j | j in 1..t], [x[i,j] | j in 1..t ])
        
     )
     /\ % select operations
     (
         ( % either the row is same as last line
            same([x[i-1,j] | j in 1..t], [x[i,j] | j in 1..t ])
            /\ 
            operations[i] = 0
            /\
            moves[i,1] = 0 % dummy move
            /\
            moves[i,2] = 0
          )
         \/
         ( % or we move a piece
           moves[i-1, 2] = moves[i, 1]
           /\
           x[i-1,moves[i,1]] = t % find the blank
           /\
           valid_moves[moves[i,1], moves[i,2]] = 1
           /\
           x[i, moves[i,2]] = t
           /\ 
           operations[i] = 1
           /\
           moves[i,1] > 0 
           /\
           moves[i,2] > 0 
           /\
           moves[i,1] != moves[i,2]
         )
     )
   )

 /\ % check_ix is the first index where we have a solution
 exists(i in 2..num_sols) (
   check_ix = i
   /\
   check[i] = 1
   /\ % no other solutions before check_ix = i
   forall(j in 2..i-1) ( check[j] = 0) 
  )
;

output 
["\npuzzle:"] ++
[
  if i mod (ceil(sqrt(int2float(t)))) = 1 then "\n" else " " endif ++
    show_int(2,puzzle[i])

  | i in 1..t
] ++ 
[
  "\noperations: ", show(operations),"\n",
  "check: ", show(check),"\n",
  "check_ix: ", show(check_ix),"\n",
  "changes: ", show(changes),
] ++
[
  if j = 1 then "\n\n" ++ show(check[i]) ++ " (move: " ++ show([moves[i,k] | k in 1..2]) ++ "):\n"  else " " endif ++
    if (j-1) mod ceil(sqrt(t)) = 0 then "\n" else "" endif ++
    show_int(2, x[i,j])
  | i in 1..num_sols, j in 1..t
] ++
["\nmoves:"]
++
[
 if j = 1 then "\n" else " " endif ++
   show_int(2, moves[i,j])
 | i in 1..num_sols, j in 1..2
]
++ ["\n"]
;

% simple problem for 15-puzzle:
%
% 16 is the blank,
%  16  2  3  4
%  1  6  7  8
%  5 10 11 12
%  9 13 14 15
%
% num_sols = 15;
% puzzle = [16, 2, 3, 4,
%           1, 6, 7, 8,
%           5,10,11,12,
%           9,13,14,15];


% From a Picat program
num_sols = 20;
puzzle = [ 
           1, 2, 3, 4,
           5, 6, 8,12,
          11,16, 7,15,
          10, 9,13,14
          ];



% from http://www.javaonthebrain.com/java/puzz15/ (Java applet)
% num_sols = 80;
% puzzle = [ 9, 2,12, 6,
%            5, 7,14,13,
%            3, 4, 1,11,
%           15,10, 7,16
%           ];

% impossible configuration: http://en.wikipedia.org/wiki/N-puzzle
%puzzle = [1,2,3,4,
%          5,6,7,8,
%          9,10,11,12,
%          13,15,14,16];


%
% data
%

%
% for 15-puzzle
%
 valid_moves = array2d(1..t, 1..t, [
  % 1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6
    0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0, % 1
    1,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0, % 2
    0,1,0,1,0,0,1,0,0,0,0,0,0,0,0,0, % 3
    0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0, % 4
    1,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0, % 5
    0,1,0,0,1,1,0,0,0,1,0,0,0,0,0,0, % 6
    0,0,1,0,0,1,0,1,0,0,1,0,0,0,0,0, % 7
    0,0,0,1,0,0,1,0,0,0,0,1,0,0,0,0, % 8
    0,0,0,0,1,0,0,0,0,1,0,0,1,0,0,0, % 9
    0,0,0,0,0,1,0,0,1,0,1,0,0,1,0,0, % 10
    0,0,0,0,0,0,1,0,0,1,0,1,0,0,1,0, % 11
    0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,1, % 12
    0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0, % 13
    0,0,0,0,0,0,0,0,0,1,0,0,1,0,1,0, % 14
    0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,1, % 15
    0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0  % 16
]);


%
% Simpler problem for 8-puzzle
%
% 1 2 3
% 4 5 6
% 7 8 9
% -> 
% 1 2 3
% 4 9 6
% 7 5 8
%
% Move is how the blank item (9) moves.

% 1 move. 9->8
%puzzle = [1,2,3, 
%          4,5,6, 
%          7,9,8];

% 2 moves. 9->5
%puzzle = [1,2,3, 
%          4,9,6, 
%          7,5,8];

% 3 moves. 9->4
%puzzle = [1,2,3, 
%          9,4,6, 
%          7,5,8];

% 4 moves. 9->1
%puzzle = [9,2,3, 
%          1,4,6, 
%          7,5,8];

% 5 moves. 9->2
%puzzle = [2,9,3, 
%          1,4,6, 
%         7,5,8];

% 6 moves. 9 -> 5 (again)
%puzzle = [2,4,3, 
%          1,9,6, 
%          7,5,8];

% 6 moves. 9->3
%puzzle = [2,3,9, 
%          1,4,6, 
%          7,5,8];

% 7 moves. 9->6
% puzzle = [2,3,6, 
%           1,4,9, 
%           7,5,8 
%           ];

% 8 moves 9->9 (again)
%puzzle = [2,3,6, 
%          1,4,8, 
%          7,5,9];

% 9 moves 9->8 (again)
% puzzle = [2,3,6, 
%          1,4,8, 
%          7,9,5];

% 10 moves 9->7
% puzzle = [2,3,6, 
%          1,4,8, 
%          9,7,5];

% 11 moves 9->4
%puzzle = [2,3,6, 
%         9,4,8, 
%         1,7,5];

% 12 moves 9->1
%puzzle = [9,3,6, 
%         2,4,8, 
%         1,7,5];


% impossible configuration
%puzzle = [1,2,3,
%          4,5,6,
%          8,7,9];


% 
% 1 2 3
% 4 5 6
% 7 8 9

% 
% valid moves for the 8-puzzle.
%

% num_sols = 8;
% % 1 2 3 4 5 6 7 8 9
% valid_moves = [|
%   0,1,0,1,0,0,0,0,0, % 1 
%   |1,0,1,0,1,0,0,0,0, % 2
%   |0,1,0,0,0,1,0,0,0, % 3 
%   |1,0,0,0,1,0,1,0,0, % 4 
%   |0,1,0,1,0,1,0,1,0, % 5 
%   |0,0,1,0,1,0,0,0,1, % 6 
%   |0,0,0,1,0,0,0,1,0, % 7 
%   |0,0,0,0,1,0,1,0,1, % 8 
%   |0,0,0,0,0,1,0,1,0  % 9
% |];
