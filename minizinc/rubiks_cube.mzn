%
% Rubik's cube (finding optimal number of moves) in MiniZinc.
%
% This model searches for optimimal number of moves for a configuration
% of Rubik's cube.
% 
% The representations has been inspired by the analysis in GAP 
% (by Martin Schönert) found at
% http://www.gap-system.org/Doc/Examples/rubik.html
% 
% Also, GAP has been used to generate some examples.
% For more about GAP (Groups, Algorithms, Programming), see
% http://www.gap-system.org/
%
% Here is the layout with the numbers representing the elements:
% 
%                +--------------+
%                |              |
%                |  1    2    3 |
%                |              |
%                |  4  top    5 |
%                |              |
%                |  6    7    8 |
%                |              |
% +--------------+--------------+--------------+--------------+
% |              |              |              |              |
% |  9   10   11 | 17   18   19 | 25   26   27 | 33   34   35 |
% |              |              |              |              |
% | 12  left  13 | 20 front  21 | 28 right  29 | 36  rear  37 |
% |              |              |              |              |
% | 14   15   16 | 22   23   24 | 30   31   32 | 38   39   40 |
% |              |              |              |              |
% +--------------+--------------+--------------+--------------+
%                |              |
%                | 41   42   43 |
%                |              |
%                | 44 bottom 45 |
%                |              |
%                | 46   47   48 |
%                |              |
%                +--------------+
%
% 
% The generators are 
%
% T (or U), move 1: ( 1, 3, 8, 6)( 2, 5, 7, 4)( 9,33,25,17)(10,34,26,18)(11,35,27,19)
% L       , move 2: ( 9,11,16,14)(10,13,15,12)( 1,17,41,40)( 4,20,44,37)( 6,22,46,35)
% F       , move 3: (17,19,24,22)(18,21,23,20)( 6,25,43,16)( 7,28,42,13)( 8,30,41,11)
% R       , move 4: (25,27,32,30)(26,29,31,28)( 3,38,43,19)( 5,36,45,21)( 8,33,48,24)
% E       , move 5: (33,35,40,38)(34,37,39,36)( 3, 9,46,32)( 2,12,47,29)( 1,14,48,27)
% B (or D), move 6: (41,43,48,46)(42,45,47,44)(14,22,30,38)(15,23,31,39)(16,24,32,40)
% 
% The group (cube) is generated in GAP with those generators:
% 
% gap> cube := Group(
%   ( 1, 3, 8, 6)( 2, 5, 7, 4)( 9,33,25,17)(10,34,26,18)(11,35,27,19),
%   ( 9,11,16,14)(10,13,15,12)( 1,17,41,40)( 4,20,44,37)( 6,22,46,35),
%   (17,19,24,22)(18,21,23,20)( 6,25,43,16)( 7,28,42,13)( 8,30,41,11),
%   (25,27,32,30)(26,29,31,28)( 3,38,43,19)( 5,36,45,21)( 8,33,48,24),
%   (33,35,40,38)(34,37,39,36)( 3, 9,46,32)( 2,12,47,29)( 1,14,48,27),
%   (41,43,48,46)(42,45,47,44)(14,22,30,38)(15,23,31,39)(16,24,32,40));
%
% cube.1 is the first generator (T), cube.2 is the second (L), etc.
%
% To generate a problem in GAP for this MiniZinc model use ListPerm(..., 48) to
% generate the list. E.g.
% 
%    gap> r := cube.1^2*cube.2^2*cube.3^2*cube.4^2*cube.5^2*cube.6^2; ListPerm(r,48);
%    (1,48,8)(2,47)(4,44)(5,45)(6,46,43)(7,42)(9,32,25)(10,15)(11,14,30)(13,29,28)(17,40,24)(18,23)(19,35,38)(20,36,21)(26,31)(34,39)
%    [ 48, 47, 3, 44, 45, 46, 42, 1, 32, 15, 14, 12, 29, 30, 10, 16, 40, 23, 35, 36, 20, 22, 18, 17, 9, 31, 27, 13, 28, 11, 26, 25, 33, 39, 38, 21, 37, 19, 34, 24, 41, 7, 6, 4, 5, 43, 2, 8 ]
% 

%
% Notes:
%
% * In this model we search for the minimum number of moves given a
%   specific problem to the 1..48 positions. This might be harder than necessary...
%
% * We use only the 6 basic moves and theirs inverses (+ a no op move), 
%   but no special combined moves beside that (such as uses of commutators 
%   or special Cubist heuristics etc).
% 
% * This is perhaps not the most effective way of solving a random setup. 
%   The interesting part is - IMHO - when one ask questions like: 
%    - Give me a legal (solvable) configuration that places "1" in position 
%      3, and "2" in position 4 and then solve it. See below for some examples.
%    - or - which is one of the advantages of most CP systems - when one want to 
%      see all possible solutions, e.g. for a given number of moves to a problem
%      instance.
%
% * I'm not really a Cubist so some of the used terms might confuse.
%

%
% Model created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%
include "globals.mzn";

int: n = 48; % number of elements

% The number of possible moves (rows)
% "God's Number" is 20, i.e. the maximal number of moves 
% that is needed. (See http://cube20.org/ for more about this.)
% int: rows = 41; 
int: rows = 21; 


% 
% Here are the 12 permutations (6 moves + their inverses + 1 no op).
% 
% int: num_moves = 12;
int: num_moves = 18; % added the 6 Move^2
array[0..num_moves, 1..n] of int: moves = array2d(0..num_moves, 1..n,
  [

   % No op: Move 0
   1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48,

  % Top (also called Upper): move 1, generator ( 1, 3, 8, 6)( 2, 5, 7, 4)( 9,33,25,17)(10,34,26,18)(11,35,27,19)
   3, 5, 8, 2, 7, 1, 4, 6, 33, 34, 35, 12, 13, 14, 15, 16, 9, 10, 11, 20, 21, 22, 23, 24, 17, 18, 19, 28, 29, 30, 31, 32, 25, 26, 27, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48,

  % T inverse (Ti)
  6, 4, 1, 7, 2, 8, 5, 3, 17, 18, 19, 12, 13, 14, 15, 16, 25, 26, 27, 20, 21, 22, 23, 24, 33, 34, 35, 28, 29, 30, 31, 32, 9, 10, 11, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48,


  % Left: move 2, generator ( 9,11,16,14)(10,13,15,12)( 1,17,41,40)( 4,20,44,37)( 6,22,46,35)
   17, 2, 3, 20, 5, 22, 7, 8, 11, 13, 16, 10, 15, 9, 12, 14, 41, 18, 19, 44, 21, 46, 23, 24, 25, 26, 27, 28, 29,  30, 31, 32, 33, 34, 6, 36, 4, 38, 39, 1, 40, 42, 43, 37, 45, 35, 47, 48,

  % Left inverse (Li)
  40, 2, 3, 37, 5, 35, 7, 8, 14, 12, 9, 15, 10, 16, 13, 11, 1, 18, 19, 4, 21, 6, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 46, 36, 44, 38, 39, 41, 17, 42, 43, 20, 45, 22, 47, 48,


  % Front: move 3, generator (17,19,24,22)(18,21,23,20)( 6,25,43,16)( 7,28,42,13)( 8,30,41,11)
  1, 2, 3, 4, 5, 25, 28, 30, 9, 10, 8, 12, 7, 14, 15, 6, 19, 21, 24, 18, 23, 17, 20, 22, 43, 26, 27, 42, 29, 41, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 11, 13, 16, 44, 45, 46, 47, 48,

  % F inverse (Fi)
  1, 2, 3, 4, 5, 16, 13, 11, 9, 10, 41, 12, 42, 14, 15, 43, 22, 20, 17, 23, 18, 24, 21, 19, 6, 26, 27, 7, 29, 8, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 30, 28, 25, 44, 45, 46, 47, 48,


  % Right: move 4, generator (25,27,32,30)(26,29,31,28)( 3,38,43,19)( 5,36,45,21)( 8,33,48,24)
  1, 2, 38, 4, 36, 6, 7, 33, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 3, 20, 5, 22, 23, 8, 27, 29, 32, 26, 31, 25, 28, 30, 48, 34, 35, 45, 37, 43, 39, 40, 41, 42, 19, 44, 21, 46, 47, 24,

  % R inverse (Ri)
  1, 2, 19, 4, 21, 6, 7, 24, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 43, 20, 45, 22, 23, 48, 30, 28, 25, 31, 26, 32, 29, 27, 8, 34, 35, 5, 37, 3, 39, 40, 41, 42, 38, 44, 36, 46, 47, 33,


  % E (Rear) move 5, generator (33,35,40,38)(34,37,39,36)( 3, 9,46,32)( 2,12,47,29)( 1,14,48,27)
  14, 12, 9, 4, 5, 6, 7, 8, 46, 10, 11, 47, 13, 48, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 1, 28, 2, 30, 31, 3, 35, 37, 40, 34, 39, 33, 36, 38, 41, 42, 43, 44, 45, 32, 29, 27,

  % E inverse (Ei)
  27, 29, 32, 4, 5, 6, 7, 8, 3, 10, 11, 2, 13, 1, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 48, 28, 47, 30, 31, 46, 38, 36, 33, 39, 34, 40, 37, 35, 41, 42, 43, 44, 45, 9, 12, 14,


  % B, Bottpm (also called D): move 6, generator (41,43,48,46)(42,45,47,44)(14,22,30,38)(15,23,31,39)(16,24,32,40)
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 22, 23, 24, 17, 18, 19, 20, 21, 30, 31, 32, 25, 26, 27, 28, 29, 38, 39, 40, 33, 34, 35, 36, 37, 14, 15, 16, 43, 45, 48, 42, 47, 41, 44, 46,

  % B inverse (Bi)
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 38, 39, 40, 17, 18, 19, 20, 21, 14, 15, 16, 25, 26, 27, 28, 29, 22, 23, 24, 33, 34, 35, 36, 37, 30, 31, 32, 46, 44, 41, 47, 42, 48, 45, 43,


  % The 6 Move^2
  % Move top^2
  8,7,6,5,4,3,2,1,25,26,27,12,13,14,15,16,33,34,35,20,21,22,23,24,9,10,11,28,29,30,31,32,17,18,19,36,37,38,39,40,41,42,43,44,45,46,47,48,

  % Move left^2
  41,2,3,44,5,46,7,8,16,15,14,13,12,11,10,9,40,18,19,37,21,35,23,24,25,26,27,28,29,30,31,32,33,34,22,36,20,38,39,17,1,42,43,4,45,6,47,48,

  % Move front^2
  1,2,3,4,5,43,42,41,9,10,30,12,28,14,15,25,24,23,22,21,20,19,18,17,16,26,27,13,29,11,31,32,33,34,35,36,37,38,39,40,8,7,6,44,45,46,47,48,

  % Move right^2
  1,2,43,4,45,6,7,48,9,10,11,12,13,14,15,16,17,18,38,20,36,22,23,33,32,31,30,29,28,27,26,25,24,34,35,21,37,19,39,40,41,42,3,44,5,46,47,8,

  % Move rear^2
  48,47,46,4,5,6,7,8,32,10,11,29,13,27,15,16,17,18,19,20,21,22,23,24,25,26,14,28,12,30,31,9,40,39,38,37,36,35,34,33,41,42,43,44,45,3,2,1,

  % Move bottom^2
  1,2,3,4,5,6,7,8,9,10,11,12,13,30,31,32,17,18,19,20,21,38,39,40,25,26,27,28,29,14,15,16,33,34,35,36,37,22,23,24,48,47,46,45,44,43,42,41,


  ]);

% Using GAP's notation (and ".i" for the inverse moves)
array[0..num_moves] of string: moves_str = array1d(0..num_moves, 
  % ["-", "T", "Ti", "L", "Li", "F", "Fi", "R", "Ri","E", "Ei", "B", "Bi"]);
  % 0   1    2     3    4     5    6     7    8    9    10    11   12      13   14   15   16   17   18
  ["-", "T", "Ti", "L", "Li", "F", "Fi", "R", "Ri","E", "Ei", "B", "Bi",   "T2","L2","F2","R2","E2","B2"]);

%
% decision variables
%

% The results of the operations, starting with the init as first row
array[1..rows, 1..n] of var 1..n: x;
array[1..n] of var 1..n: init; % init array

% index of the minimum number of moves
% Note: it's 1+number of moves.
var 1..rows: check_ix;

% the operations:
array[1..rows] of var 0..num_moves: operations;


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

% Copy an array to a decision variable array
predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
    "cp1d: arrays x and y must have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


% solve minimize check_ix;
% solve satisfy;
% ann: var_select;
% ann: val_select;
% solve :: int_search(
%         operations
%         % [x[i,j] | i in 1..rows, j in 1..n]
%         % ++ operations
%         % [x[rows-i+1,j] | i in 1..rows, j in 1..n]
%         % ++[operations[rows-i+1] | i in 1..rows]
%         , 
%         smallest, % max_regret, 
%         indomain_min,
%         complete) 
%     minimize check_ix;
%     % satisfy;

solve :: int_search(
        operations
        % [x[i,j] | i in 1..rows, j in 1..n]
        % ++ operations
        % [x[rows-i+1,j] | i in 1..rows, j in 1..n]
        % ++[operations[rows-i+1] | i in 1..rows]
        , 
        anti_first_fail, % smallest,
        indomain_min, % indomain_min,
        complete) 
    minimize check_ix;
    % satisfy;


constraint

   operations[1] = 0 /\

   % initialize the first row of matrix.
   forall(j in 1..n) (
      x[1, j] = init[j]
      /\
      x[rows,j] = j
   )

   /\ 
   forall(i in 1..rows) (
     alldifferent([x[i,j] | j in 1..n]) % :: domain
   )

   /\
   forall(i in 2..rows) (
      let { 
        var 0..num_moves: m 
      } in
      % permutation3([x[i-1,k] | k in 1..n],[moves[m,k] | k in 1..n],[x[i,k] | k in 1..n])  % :: domain
      % inverse approach
      permutation3([x[i,k] | k in 1..n],[moves[m,k] | k in 1..n],[x[i-1,k] | k in 1..n])  % :: domain
      /\ 
      operations[i] = m
   )

   /\ % there must be some sequence where we find the goal sequence
   forall(j in 1..n) ( x[check_ix,j] = j )

   /\ % Symmetry breaking: 
      % all the further entries after check_ix steps must also be the solution.
   forall(k in 2..rows) (
     k > check_ix -> forall(j in 1..n) (x[k,j] = j) 
                     /\ operations[k] = 0
     )

   /\
   % Symmetry breaking: this seems to be a good booster
   % (checking for real moves)
   forall(i in 2..rows) (
      i < check_ix <-> operations[i] > 0
   )

;


%
% Problem instances.
%
% Note: I place these in a "constraint" section to be able to use anonymous 
% placements (i.e. "_") for certain problem instances.
%
% Instruction: Uncomment one cp1d() line to solve that instance.
% 
constraint
   % Skeleton
   % cp1d(init, [ _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _])

  % Skeleton
  % 
  %            1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,
  % cp1d(init, [ _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _,

  %            % 25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48    
  %              _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _])



  % From: http://www.gap-system.org/Doc/Examples/rubik.html
  % gap> ListPerm((17,19)(11,8)(6,25)(7,28)(18,21) , 48);
  % cp1d(init, [ 1, 2, 3, 4, 5, 25, 28, 11, 9, 10, 8, 12, 13, 14, 15, 16, 19, 21, 17, 20, 18, 22, 23, 24, 6, 26, 27, 7, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48 ])


  % Find a setup where "1" is in the third position (and disregarding the other positions)
  % and then solve it.
  % init: [6, 4, 1, 7, 2, 8, 5, 3, 17, 18, 19, 12, 13, 14, 15, 16, 25, 26, 27, 20, 21, 22, 23, 24, 33, 34, 35, 28, 29, 30, 31, 32, 9, 10, 11, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48]
  % Move: Ti
  % cp1d(init, [ _, _, 1, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _])


  % Find a setup where 1<->3 are swapped
  % init: [3, 4, 1, 42, 2, 41, 36, 38, 33, 23, 22, 12, 21, 8, 5, 24, 16, 29, 32, 28, 20, 30, 18, 17, 48, 34, 35, 13, 31, 11, 39, 40, 9, 10, 27, 45, 37, 14, 15, 19, 43, 7, 6, 26, 47, 25, 44, 46]
  % Moves: TiFiBFiR
  % cp1d(init, [ 3, _, 1, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _])

  % Swap 1<->3 and 2<->4
  % init: [3, 4, 1, 2, 7, 17, 20, 22, 33, 34, 6, 12, 29, 14, 15, 32, 11, 13, 16, 36, 5, 38, 23, 8, 41, 18, 35, 26, 31, 25, 28, 30, 9, 10, 27, 45, 37, 43, 39, 40, 48, 42, 19, 44, 21, 46, 47, 24]
  % 6 moves: TiLiTTLR
  % cp1d(init, [ 3, 4, 1, 2, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _])

  % swap 1<->3, 2<->4, and 5<->7
  % init: [3, 4, 1, 2, 7, 8, 5, 6, 33, 34, 19, 12, 29, 14, 15, 32, 25, 26, 11, 36, 21, 38, 23, 24, 17, 18, 35, 28, 13, 30, 31, 1, 9, 10, 27, 20, 37, 22, 39, 40, 48, 42, 43, 44, 45, 46, 47, 41]
  % 7 moves: TiRiLiTTLR
  % cp1d(init, [ 3, 4, 1, 2, 7, _, 5, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _])


  % swap 1<->48
  % Moves: EE or EiEi
  % cp1d(init, [ 48, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, 1])

  % RLiFiFiEiEiLiR
  % cp1d(init, [ 48, 47,46, 45, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, 4, 3, 2, 1])


  % reverse
  % cp1d(init, [48,47,46,45,44,43,42,41,40,39,38,37,36,35,34,33,32,31,30,29,28,27,26,25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1])


  % swap 1<->2
  % No solution (it's not possible to swap a corner and a middle position)
  % cp1d(init, [2, 1, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _])


  % TLTL:
  % 4 moves: TLTL (or TLTiTi)
  % cp1d(init, [ 6, 4, 1, 7, 2, 33, 20, 22, 17, 18, 3, 26, 15, 25, 12, 14, 27, 13, 16, 44, 21, 46, 23, 24, 41, 34, 35, 28, 29, 30, 31, 32, 9, 10, 11, 36, 5, 38, 39, 8, 40, 42, 43, 37, 45, 19, 47, 48 ])
  % /\ check_ix = 5 % for solve satisfy)
  
  % TLFREB
  % Found: TLFREB
  % cp1d(init, [ 33, 34, 25, 12, 26, 9, 18, 17, 27, 37, 1, 10, 23, 41, 44, 46, 35, 7, 6, 42, 31, 40, 20, 30, 11, 5, 8, 45, 39, 43, 28, 38, 19, 2, 3, 47, 4, 48, 36, 22, 14, 13, 24, 15, 21, 16, 29, 32 ])

  % (TLFREB)^-1
  % Found: BiEiRiFiLiTi (only solution with 6 moves)
  %    Gecode: 2.7s (451 failures)   Gecode + free: 2.1s (451 failures)
  %    Chuffed: 9.1s
  %    or-tools: 9.4s (3492 fauilures)
  cp1d(init, [ 11, 34, 35, 37, 26, 19, 18, 27, 6, 12, 25, 4, 42, 41, 44, 46, 8, 7, 33, 23, 45, 40, 13, 43, 3, 5, 9, 31, 47, 24, 21, 48, 1, 2, 17, 39, 10, 32, 29, 22, 14, 20, 30, 15, 28, 16, 36, 38 ])
  % /\ check_ix = 7

  % T^2*L^2*F^2*R^2*E^2*B^2
  % operations: [0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 0, 0, 0, 0, 0, 0, 0]
  % ["-", "T", "T", "L", "L", "F", "F", "R", "R", "E", "E", "B", "B", "-", "-", "-", "-", "-", "-", "-"]
  %    Gecode: 28.9s (47876): Moves: T2L2F2R2E2B2
  % cp1d(init, [ 48, 47, 3, 44, 45, 46, 42, 1, 32, 15, 14, 12, 29, 30, 10, 16, 40, 23, 35, 36, 20, 22, 18, 17, 9, 31, 27, 13, 28, 11, 26, 25, 33, 39, 38, 21, 37, 19, 34, 24, 41, 7, 6, 4, 5, 43, 2, 8 ])

  % gap> ListPerm(t*t*t*l*l*l,48);
  % Found moves: TiLi
  %    Gecode: 1.8s (3 failures)
  % cp1d(init, [ 35, 37, 40, 7, 2, 8, 5, 3, 1, 18, 19, 15, 10, 16, 13, 11, 25, 26, 27, 4, 21, 6, 23, 24, 33, 34, 46, 28, 29, 30, 31, 32, 14, 12, 9, 36, 44, 38, 39, 41, 17, 42, 43, 20, 45, 22, 47, 48 ])

  % gap> ListPerm(b*b*b*r*r*r*b,48);
  % Found moves: BiRiB
  %     Gecode: 1.8s  (23 failures)
  % cp1d(init, [ 1, 2, 19, 4, 21, 6, 7, 32, 9, 10, 11, 12, 13, 3, 15, 16, 17, 18, 48, 20, 47, 22, 23, 24, 38, 28, 25, 39, 26, 30, 31, 46, 8, 34, 35, 5, 37, 40, 29, 27, 41, 42, 43, 44, 45, 33, 36, 14 ])


  % gap> ListPerm(b*b*b*r*r*r*b*f*t*l*l*r,48);
  % Found moves: BiRiBFTLiLiR
  % All possible moves with 8 steps:
  %  BiRiBFTLiLiR
  %  BiRiBFTLiRLi
  %  BiRiBFTLLR
  %  BiRiBFTLRL
  %  BiRiBFTRLiLi
  %     Gecode: 16.8s (17764 failures)  BiRiBFTRL2 (with the 6 move^2 moves)
  %             14.5s  (16391 failures) with move^2 moves and rows = 21
  % cp1d(init, [ 38, 36, 8, 2, 23, 40, 26, 30, 48, 34, 46, 13, 44, 33, 10, 41, 14, 5, 24, 15, 47, 16, 37, 35, 43, 42, 19, 39, 18, 1, 28, 6, 25, 29, 32, 7, 20, 17, 31, 3, 22, 12, 9, 4, 21, 27, 45, 11 ])
  % % /\ check_ix = 9 % for solve satisfy


  % From A Mathematical Approach To Solving Rubik's Cube
  % http://www.math.ubc.ca/~cass/courses/m308/projects/rtran/rtran.pdf
  % page 4
  % """
  % The scrambled cube is created from the initial position
  % using the following moves:
  % U*L*D*R*F*B^-1*U^-1L^-1 (ie. Uclk, Lclk, Dclk, Rclk, Fclk, Bcnt, Ucnt, Lcnt).
  % """
  % 
  % There are two solutions with optimal 8 moves:
  %  - LTEFiRiBiLiTi
  %  - LTFiERiBiLiTi
  %     Gecode: 1:01min (71190 failures) Found LTFiERiBiLiTi
  % cp1d(init, [48,21,17,13,18,43,37,41,32,20,30,44,39,46,45,9,24,12,22,47,26,35,42,25,16,7,6,5,4,19,29,27,11,28,38,10,15,3,2,14, 1,23,8,31,36,40,34,33])
  %   %  /\ check_ix = 9

  % 
  % change 18 <-> 23 (front) and 26 <-> 31 (right) and let other positions be whatever.
  % 
  % Here are the different 4 moves (with different inits)
  % - FiFiRR
  % - FiFiRiRi
  % - FFRR
  % - FFRiRi
  % - RRFF
  % - RRFiFi
  % - RiRiFF
  % 
  %            1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,
  % cp1d(init, [ _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _,23, _, _, _, _,18, _,
  %
  %            %25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48    
  %              _,31, _, _, _, _,26, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _])
  % % /\ check_ix = 5

  % 
  % change 18 <-> 23 (front) and keep the other positions intact.
  % This is NOT possible.
  %          
  % cp1d(init, [1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,23,19,20,21,22,18,24,
  %            25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48]) 

  % 
  % change 18 <-> 23 (front) and 26 <-> 31 (right) and keep the other positions intact.
  % This is NOT possible.
  % 
  %         
  % cp1d(init, [1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,23,19,20,21,22,18,24,
  %            25,31,27,28,29,30,26,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48]) 


  % Make these 12 moves: TLEiLiRRiRiFEBTiBi
  % cp1d(operations, [0,1,3,10,4,7,8,8,5,9,11,2,12,0,0,0,0,0,0,0,0])
  % which generated this init:
  %   Gecode found these optimal moves TLEiLiRiFETi in 51.2s (61504 failures)
  % cp1d(init, [6, 23, 22, 34, 28, 14, 7, 33, 17, 2, 40, 47, 5, 24, 12, 38, 46, 18, 3, 26, 45, 48, 20, 35, 27, 21, 16, 31, 29, 1, 4, 25, 41, 42, 11, 36, 39, 19, 44, 43, 32, 13, 9, 37, 10, 30, 15, 8])

  % Make these 3 moves: T2B2F2
  % cp1d(operations, [0,13,18,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
  %   Gecode found T2B2F2 in 0.81s (40 failures)
  % cp1d(init, [41, 42, 43, 5, 4, 3, 2, 1, 16, 26, 27, 12, 28, 11, 31, 32, 33, 34, 35, 21, 20, 38, 39, 40, 9, 10, 30, 13, 29, 14, 15, 25, 24, 23, 22, 36, 37, 19, 18, 17, 48, 47, 46, 45, 44, 6, 7, 8])

  %                   13 18 15 3 7 17 2  11  
  % make these moves: T2 B2 F2 L R E2 Ti B
  % cp1d(operations, [0,13,18,15,3,7,17,2,11,0,0,0,0,0,0,0,0,0,0,0,0])
  %    Gecode found T2B2F2LRE2TiB in 12:40min (1168058 failures)
  % cp1d(init, [9, 45, 27, 37, 20, 11, 44, 25, 35, 12, 17, 18, 34, 24, 28, 38, 6, 15, 8, 2, 42, 48, 10, 46, 19, 13, 33, 23, 39, 40, 29, 22, 3, 31, 1, 47, 7, 41, 26, 43, 32, 4, 14, 21, 36, 30, 5, 16])

;

output 
[
  "\ninit: ", show(init), "\n",
  "check_ix: ", show(check_ix), "\n", 
  % "operations: ", show(operations), "\n",
  % show([moves_str[fix(operations[i])] | i in 1..rows]),
  "\nMoves: ",
] ++
[
  let {
    int: m = fix(check_ix),
    int: p = fix(operations[i])
  } in
  if p > 0 /\ i <= m then
    show(moves_str[p])
  else 
    ""
  endif
  | i in 1..rows
]
++
[
  if i > 1 /\ fix(operations[i]) > 0 then
    if j = 1 then "\n" ++  
       show(operations[i]) ++ "(" ++ show(moves_str[fix(operations[i])]) ++ "): "  
     else " " endif ++
    show(x[i,j])
  else 
     ""
  endif
  | i in 1..rows, j in 1..n
] ++ ["\n"]
;


