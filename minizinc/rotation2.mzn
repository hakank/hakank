%
% Rotation permutation puzzle in MiniZinc.
%
% From GAP mailing list
% http://www-groups.dcs.st-and.ac.uk/~gap/ForumArchive/Harris.1/Bob.1/Re__GAP_.59/1.html
% """
% Since you asked about what the puzzle actually is, the fellow who posted it
% at rec.puzzles (Kevin Buzzard <buzzard@ic.PUZZLE.ac.ELZZUP.uk>) had found it
% on his nokia cell phone. It is called "rotation" at Nokia's web site
% http://www.nokia.com/games
% I think this variant is level 5.
%
% The puzzle is a 4x4 square of numbers. There are four operations, each of
% which involves rotating the numbers in a 3x3 square clockwise. So, in the
% diagram below, one move is the cycle (1,2,3,7,11,10,9,5). The numbers
% maintain orientation-- they don't rotate; if they did, that would add
% another layer of complexity for the solver.
%
%      1  2  3  4
%      5  6  7  8
%      9 10 11 12
%     13 14 15 16
%
% Anyone who's interested in an archive of the discussion ofthis puzzle (it's
% about a 40K byte text file), let me know. The discussion focuses primarily
% on finding minimal move sequences to swap two given tiles.
%
% There's a very well done java applet for a similar puzzle at
% http://www.microprizes.com/mp52.htm 
% """

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";
int: n = 16;

% These are the four rotations, i.e. the index for the elements involved.
int: num_moves = 4;
array[0..num_moves, 1..n] of int: moves = array2d(0..num_moves, 1..n, 
  [
   1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, % no op

   % 1, 2, 3,  7, 11, 10,  9,  5, % move 1 (around 6)
   5, 1, 2, 4, 9, 6, 3, 8,10,11, 7,12,13,14,15,16,

   % 2, 3, 4,  8, 12, 11, 10,  6, % move 2 (around 7)
   1, 6, 2, 3, 5,10, 7, 4, 9,11,12, 8,13,14,15,16,

   % 5, 6, 7, 11, 15, 14, 13,  9, % move 3 (around 10)
   1, 2, 3, 4, 9, 5, 6, 8,13,10, 7,12,14,15,11,16,

   % 6, 7, 8, 12, 16, 15, 14, 10, % move 4 (around 11)
   1, 2, 3, 4, 5,10, 6, 7, 9,14,11, 8,13,15,16,12,
]);

array[0..num_moves] of string: moves_str = array1d(0..num_moves, ["-","1","2","3","4"]); 

% The number of rows
% Note: One have to change this for every problem
int: rows = 20;

% The results of the operations, starting with the init as first row
array[1..rows, 1..n] of var 1..n: x;

array[1..n] of var 1..n: init; 

% Where is the solution?
var 2..rows: check_ix;

% the operations: 0: same, 1..4: rotations
array[1..rows] of var 0..num_moves: ops;


% permutation3(a,p,b)
%   b is the permutation of a given the permutation p
%
predicate permutation3(array[int] of var int: a,
                       array[int] of var int: p,
                       array[int] of var int: b) =
   forall(i in index_set(a)) (
       b[i] = a[p[i]]
   )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


% solve satisfy;
% solve minimize check_ix;
solve :: int_search(
        % [x[k, i] | k in 1..rows, i in 1..n] ++ ops, 
        % ops,
        [ops[rows-i+1] | i in 1..rows],
        smallest, % max_regret, 
        indomain_min, 
        complete) 
    minimize check_ix;
    % satisfy;


% general constraints
constraint
  % init
  forall(j in 1..n) (
     x[1,j] = init[j]
  )
  /\
  ops[1] = 0
  /\
  forall(i in 1..rows) (
     alldifferent([x[i,j] | j in 1..n]) % :: domain
  )

  /\
  forall(i in 2..rows) (
    let {
      var 0..num_moves: m
    } in
    permutation3([x[i-1,k] | k in 1..n],[moves[m,k] | k in 1..n],[x[i,k] | k in 1..n])  % :: domain
    % permutation3([x[i,k] | k in 1..n],[moves[m,k] | k in 1..n],[x[i-1,k] | k in 1..n])  % :: domain
    /\ 
    ops[i] = m
 )

   % /\ % there must be some sequence 1..n (the goal)
   % exists(i in 1..rows) (
   %    forall(j in 1..n) ( x[i,j] = j )
   %    /\
   %    check_ix = i
   %    /\ % and all the further steps must also be the solution
   %       % (symmetry breaking)
   %    forall(k in i+1..rows) (
   %      forall(j in 1..n) (x[k,j] = j)
   %      /\ ops[k] = 0
   %    )
   % )
   /\
   forall(j in 1..n) ( x[check_ix,j] = j )

   /\ % Symmetry breaking: 
      % all the further entries after check_ix steps must also be the solution.
   forall(k in 2..rows) (
      k > check_ix -> (forall(j in 1..n) (x[k,j] = j) 
                       /\ ops[k] = 0)
   )

   /\ % and this seems to be a good booster
   forall(i in 2..rows) (
      i < check_ix -> ops[i] > 0
   )
;


%
% Problem instances
%
constraint

   % just a (1-rotation around 6)
   %  2  3  7  4  
   %  1  6 11  8 
   %  5  9 10 12 
   % 13 14 15 16 

   % One 1-rotation:
   % cp1d(init, array1d(1..n, [2, 3, 7, 4, 1, 6, 11, 8, 5,  9, 10, 12, 13, 14, 15, 16]))

   % 2,1
   % cp1d(init, array1d(1..n, [2, 7, 4, 8, 1, 3, 11, 12, 5, 6, 9, 10, 13, 14, 15, 16]))

   % 1,2,1
   % cp1d(init, array1d(1..n, [7,4,11,8,2,3,9,12,1,5,6,10,13,14,15,16]))

   % 1,2,3
   % cp1d(init, array1d(1..n, [ 3, 4, 11, 8, 1, 2, 10, 12, 6, 5, 7, 15, 9, 13, 14,16 ]))
   
   % 1,2,3,2,2,3
   % cp1d(init, array1d(1..n, [ 8, 12, 7, 15, 1, 4, 2, 10, 3, 6, 11, 14, 5, 9, 13, 16 ]))
   
   % 4,4,4,1,2,3,2,2,3
   cp1d(init, array1d(1..n, [  8, 12, 7, 15, 1, 14, 16, 13, 3, 10, 11, 9, 5, 2, 4, 6 ]))

   % Random puzzle
   % cp1d(init, array1d(1..n, [  2,  4, 16, 11, 15,  7, 12,  1, 8,  9, 14,  3,  10,  5, 13,  6 ]))

   % swap 15<->16 (GAP says 53 moves, but that's probably not optimal )
   % cp1d(init, array1d(1..n, [ 1,2,3,4,5,6,7,8,9,10,11,12,13,14,16,15 ]))
;





output 
[
  "\ninit: ", show(init), "\n",
  "check_ix: ", show(check_ix), "\n", 
  "ops: ", show(ops) ++ "\n" ++
  "Moves: "
]
++
[
  let {
   int: m = fix(ops[i])
  } in
  if i > 0 /\ m > 0 then
    show(moves_str[m])
  else 
    ""
  endif
  | i in 1..rows
]
++
[
  if i = 1 then "\n" ++  show(ops[k]) ++ ": "  else " " endif ++
   show(x[k,i])
  | k in 1..rows, i in 1..n
] ++ ["\n"]
;
