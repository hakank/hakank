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
int: n = 4; % we will use 2*n below
% int: t = 2*n; % t for twice

% These are the four rotations, i.e. the index for the elements involved.
array[1..4, 1..n*2] of int: rotations = array2d(1..4, 1..n*2, 
  [
   1, 2, 3,  7, 11, 10,  9,  5,
   2, 3, 4,  8, 12, 11, 10,  6,
   5, 6, 7, 11, 15, 14, 13,  9,
   6, 7, 8, 12, 16, 15, 14, 10,
]);


% The fixed (fixated) numbers for each rotation, i.e.
% those not in rotations[ix]
array[1..4, 1..n*2] of int: fixated = array2d(1..4, 1..n*2, 
  [
   4, 6, 8, 12, 13, 14, 15, 16,
   1, 5, 7,  9, 13, 14, 15, 16,
   1, 2, 3,  4,  8, 10, 12, 16,
   1, 2, 3,  4,  5,  9, 11, 13,
]);


% The number of rows
% Note: One have to change this for every problem
int: rows = 12;

% The results of the operations, starting with the init as first row
array[1..rows, 1..n*n] of var 1..n*n: x;

array[1..n*n] of var 1..n*n: init; % init array

% Where is the solution?
array[1..rows] of var 0..1: check;
var 2..rows: check_ix;

% the operations: 0: same, 1..4: rotations
array[1..rows] of var 0..4: operations;


%
% Rotate:
%
%  array y is a rotated version of x where ix is the
%  index in the rotation matrix rot.
%
predicate rotate(array[int] of var int: x, 
                 array[int] of var int: y, 
                 array[int,int] of int: rot,
                 var int: ix % index of rotation
        ) =
   let { 
     int: n = card(index_set(x)),
     int: m = card(index_set_2of2(rot)),
   }
   in
   % rotate all elements in the rotate table,
   forall(i in 2..m) (
      y[rot[ix,i]] = x[rot[ix,i-1]]
   )
   /\ % rotate around the corner
   y[rot[ix,1]] = x[rot[ix,m]]

   /\ % fixate the fixed (untouched) numbers.
   forall(i in 1..m) (
      y[fixated[ix,i]] = x[fixated[ix,i]]
   )

   %/\ % fixated numbers (extra)
   %sum(i in 1..n*2) (bool2int(x[i] = y[i])) = m

;

% 
% array y is the same as array x
%
predicate same(array[int] of var int: x, array[int] of var int: y) =
   let { 
     int: n = card(index_set(x))
   }
   in
    forall(i in 1..n) ( y[i] = x[i] )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


% solve satisfy;
% solve minimize check_ix;
solve :: int_search([
        x[k, i] | k in 1..rows, i in 1..n*n] ++ operations, 
        max_regret, 
        indomain_min, 
        complete) 
    minimize check_ix;
    % satisfy;

constraint

   % problem instances: just a (1-rotation around 6)
   %  2  3  7  4  
   %  1  6 11  8 
   %  5  9 10 12 
   % 13 14 15 16 

   % One 1-rotation:
   % cp1d(init, array1d(1..n*n, [2, 3, 7, 4, 1, 6, 11, 8, 5,  9, 10, 12, 13, 14, 15, 16]))

   % 2,1
   % cp1d(init, array1d(1..n*n, [2, 7, 4, 8, 1, 3, 11, 12, 5, 6, 9, 10, 13, 14, 15, 16]))

   % 1,2,1
   % cp1d(init, array1d(1..n*n, [7,4,11,8,2,3,9,12,1,5,6,10,13,14,15,16]))

   % 1,2,3
   % cp1d(init, array1d(1..n*n, [ 3, 4, 11, 8, 1, 2, 10, 12, 6, 5, 7, 15, 9, 13, 14,16 ]))
   
   % 1,2,3,2,2,3
   % cp1d(init, array1d(1..n*n, [ 8, 12, 7, 15, 1, 4, 2, 10, 3, 6, 11, 14, 5, 9, 13, 16 ]))
   
   % 4,4,4,1,2,3,2,2,3
   cp1d(init, array1d(1..n*n, [  8, 12, 7, 15, 1, 14, 16, 13, 3, 10, 11, 9, 5, 2, 4, 6 ]))

   % Random puzzle
   % cp1d(init, array1d(1..n*n, [  2,  4, 16, 11, 15,  7, 12,  1, 8,  9, 14,  3,  10,  5, 13,  6 ]))

   % swap 15<->16 (GAP says 53 moves, but that's probably not optimal )
   % cp1d(init, array1d(1..n*n, [ 1,2,3,4,5,6,7,8,9,10,11,12,13,14,16,15 ]))
;


constraint
   check[1] = 0
   /\
   operations[1] = 0
   /\

   % initialize the first row of matrix.
   forall(i in 1..n*n) (
      x[1, i] = init[i]
   )

   /\ % ensure that we just all 1..n*n each row
   forall(k in 1..rows) (
      alldifferent([x[k, i] | i in 1..n*n]) % :: domain
   )
;

% select the operations
constraint
   forall(k in 2..rows) (
      (
         ( % either two rows are same ...
            same([x[k-1, i] | i in 1..n*n], [x[k, i] | i in 1..n*n ])
            /\ 
            operations[k] = 0
         )
         \/
         ( % ... or one of the rotates 1..4
            exists(ix in 1..4) ( 
               rotate([x[k-1,i] | i in 1..n*n], [x[k, i] | i in 1..n*n ], rotations, ix) 
              /\ 
              operations[k] = ix
            )
         )
     )
   )
;


% We got a solution (+ symmetry breaking)
constraint
    exists(k in 2..rows) (
      forall(i in 1..n*n) (
         x[k, i] = i
      )
      /\
      check_ix = k
      /\
      check[k] = 1

      % symmetry breaking 
      /\ % no other solutions before check_ix = i
      forall(j in 2..k-1) ( check[j] = 0)
      /\ % no other solutions after check_ix = i
      forall(j in k+1..rows) ( check[j] = 0) 
    )
;

% further symmetry breaking and optimizations
constraint
   sum(check) = 1
   /\ % another efficiency constraint: no two rows may be alike 
       % unless it is the solution.
    forall(k in 2..rows) (
       (check[k] = 0 /\ k < check_ix)
       -> 
        not forall(i in 1..n*n) (x[k-1, i] = x[k,i])      
    )
   /\ % after we found a solution, all solution, all are same 
   forall(k in 1..rows) (        
       k > check_ix -> (operations[k] = 0  /\ check[k] = 0)
   )
;

output 
[
  "\ninit: ", show(init), "\n",
  "check: ", show(check), "\n",
  "check_ix: ", show(check_ix), "\n", 
  "operations: ", show(operations) ++ "\n" 
]
++
[
  if i = 1 then "\n" ++  show(operations[k]) ++ ": "  else " " endif ++
   show(x[k,i])
  | k in 1..rows, i in 1..n*n
] ++ ["\n"]
;
