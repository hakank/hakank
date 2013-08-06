%
% M12 problem in MiniZinc.
%
% See 
% Igor Kriz and Paul Siegel: 
%     Rubik's Cube Inspired Puzzles Demonstrate Math's "Simple Groups"
% http://www.sciam.com/article.cfm?id=simple-groups-at-play
%
% Programs:
%  http://www.math.lsa.umich.edu/~ikriz/
%  http://www.sciam.com/article.cfm?id=puzzles-simple-groups-at-play
%
% This model implements the M12 puzzle:
%  - length is 12 (2*6)
%  - the two operations are 
%      * merge (shuffle) and 
%      * inverse (reverse)
%  - some init configuration

%
% For a group theoretic solution of the M12 puzzle using the abstract algebra system GAP, 
% see http://www.hakank.org/group_theory/M12_gap.txt
% It is presented in the (Swedish) blog post
% "Gruppteoretisk lösning av M12 puzzle i GAP" (Group theoretical solution of the M12 puzzle in GAP)
% http://www.hakank.org/webblogg/archives/001226.html

% Note: This version use the two conversion permutations instead of
%       operation predicates. This is often faster than M12.mzn.

%
% Model created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%
include "globals.mzn";

int: t = 12;

% The number of rows
% Note: One have to change this for every problem
int: rows = 30;

int: num_moves = 2;
array[0..num_moves, 1..t] of int: moves = array2d(0..num_moves, 1..t,
  [
    1,2,3,4,5,6,7,8,9,10,11,12, % move 0 (nop)
    1,3,5,7,9,11,12,10,8,6,4,2, % 1,12,2,11,3,10,4,9,5,8,6,7, % move 1 merge
    12,11,10,9,8,7,6,5,4,3,2,1, % move 2 reverse (inverse)
  ]);

array[0..num_moves] of string: moves_str = array1d(0..num_moves, ["-", "M", "R"]);

% The results of the operations, starting with the init as first row
array[1..rows, 1..t] of var 1..t: x;
array[1..t] of var 1..t: init; % init array


% is this row the same as the original?
% array[1..rows] of var 0..1: check;
var 2..rows: check_ix;

% the operations: 0: same, 1: shuffle, 2: reverse, 3: rotate
array[1..rows] of var 0..2: operations;

%
% permutation3(a,p,b) 
%
% get the permutation from a <-> b using the permutation p.
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


% solve minimize check_ix;
% solve satisfy;
% ann: var_select;
% ann: val_select;
solve :: int_search(
        % [x[i,j] | i in 1..rows, j in 1..t]
        %++ 
        operations
        , 
        anti_first_fail, % smallest, 
        indomain_min, 
        complete) 
    minimize check_ix;
    % satisfy;


constraint

   operations[1] = 0 /\

   % initialize the first row of matrix.
   forall(j in 1..t) (
      x[1, j] = init[j]
   )

   /\
   forall(i in 1..rows) (
     alldifferent([x[i,j] | j in 1..t])  % :: domain
   )

   /\
   forall(i in 2..rows) (
      let { 
        var 0..num_moves: m 
      } in
      permutation3([x[i-1,k] | k in 1..t],[moves[m,k] | k in 1..t],[x[i,k] | k in 1..t])  % :: domain
      /\ 
      operations[i] = m
   )

   % /\ % there must be some sequence 1..n (the goal)
   % exists(i in 1..rows) (
   %    forall(j in 1..t) ( x[i,j] = j )
   %    /\
   %    check_ix = i
   %    /\ % and all the further steps must also be the solution
   %       % (symmetry breaking)
   %    forall(k in i+1..rows) (
   %      forall(j in 1..t) (x[k,j] = j)
   %      /\ operations[k] = 0
   %    )
   % )

   /\ % another version of the exists loop.
      % this direct approach (without exists) is often faster
   forall(j in 1..t) ( 
       x[check_ix,j] = j 
   )

   /\ % Symmetry breaking: 
      % all the further entries after check_ix steps must also be the solution.
   forall(k in 2..rows) (
      k > check_ix -> (forall(j in 1..t) (x[k,j] = j) 
                       /\ operations[k] = 0)
   )


   /\ % this seems to be a good booster
   forall(i in 2..rows) (
      i < check_ix -> operations[i] > 0
   )
;


output 
[
  "\ninit: ", show(init), "\n",
  "check_ix: ", show(check_ix), "\n", 
  "operations: ", show(operations), "\n",
  show([moves_str[fix(operations[i])] | i in 1..rows]),"\n"
]
++
[
  if i > 0 /\ fix(operations[i]) > 0 then
    if j = 1 then "\n" ++  show(operations[i]) ++ "(" ++ show(moves_str[fix(operations[i])]) ++ "): "  else " " endif ++
     show(x[i,j])
  else 
    ""
  endif
  | i in 1..rows, j in 1..t
] ++ ["\n"]
;


%
% Problem instances
%
constraint

   % 
   % Note: The timings assume rows = 30
   %

   % cp1d(init, [10,5,4,7,1,2,8,3,12,11,9,6]) % this is random generated from M12proj.exe. check_ix = 16 (Chuffed: 1.3s)
   cp1d(init, [10,8,6,12,5,2,1,4,11,7,9,3]) % another generated from M12proj.exe. harder  check_ix = 23 (Chuffed: 6.9s)
   % cp1d(init, [11,7,3,8,5,2,12,1,9,10,4,6]) % another generated from M12proj.exe check_ix=23 (Chuffed: 9.3s)
   % cp1d(init, [7,5,8,3,1,11,2,9,4,12,6,10]) % generated from M12proj.exe check_ix=20 (Chuffed: 2.7s)
   % cp1d(init, [8,11,6,1,10,9,4,3,12,7,2,5]) % check_ix=28 (Chuffed: 1:29min)
   % cp1d(init, [1,4,9,3,11,6,8,5,10,2,7,12]) % check_ix > 30 (Chuffed: ?)
   % cp1d(init, [3,8,6,12,4,7,5,11,1,10,9,2]) % check_ix=20 (Chuffed: 2.5s)

   % cp1d(init, [4,1,10,7,9,12,3,6,5,2,11,8]) % generated by M2I1M check_ix = 5 (Chuffed: 0.2s)
   % cp1d(init, [7,1,8,9,12,5,3,10,4,11,6,2]) % generated by [r,s,s,s,s,r,s,s,r,r,s] (Chuffed: 0.4s)
   % cp1d(init, [5,6,11,10,8,2,3,12,7,4,9,1]) % check_ix 14 (Chuffed: 0.6s)
   % cp1d(init, [5,6,10,4,1,11,9,2,12,8,3,7]) % check_ix 13 (Chuffed: 0.6s)
   % cp1d(init, [3,4,6,10,11,1,9,7,8,2,12,5]) % check_ix 23 (Chuffed: 10.5s)
   % cp1d(init, [1,12,2,11,3,10,4,9,5,8,6,7]) % check_ix 2 (Chuffed: 0.3s)
   % cp1d(init, [1,4,7,10,12,9,6,3,2,5,8,11]) % check_ix 4 (Chuffed: 0.3s)
   % cp1d(init, [11,2,9,7,1,10,6,5,8,3,12,4]) % sssrsss (Chuffed: 0.3s)
   % cp1d(init, [12,11,10,9,8,7,6,5,4,3,2,1]) % check_ix 2 (Chuffed: 0.3s )
;
