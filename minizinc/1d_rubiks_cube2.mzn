% 
% 1D Rubik's Cube in MiniZinc.
% 
% From
% http://www.mail-archive.com/programming@jsoftware.com/msg05817.html
% """
% 1D Rubik's Cube
%
% Oleg Kobchenko
% Mon, 11 Jun 2007 19:09:55 -0700
%
% I found an interesting game, as found on Andrew Nikitin's 
% MSX-BASIC page http://nsg.upor.net/msx/basic/basic.htm ,
% and I am not sure if its solver has been given as a puzzle.
% Here it goes.
%
% 1D Rubik's Cube is a line of 6 numbers with
% original position:
%
%   1 2 3 4 5 6
%
% which can be rotated in 3 different ways
% in groups of four:
%     _______                _______
%    (1 2 3 4)5 6  --(0)->  (4 3 2 1)5 6
%       _______                _______
%     1(2 3 4 5)6  --(1)->   1(5 4 3 2)6
%         _______                _______
%     1 2(3 4 5 6) --(2)->   1 2(6 5 4 3)
%
% Given a scrambled line, return the shortest sequence of 
% rotations to restore the original position.
%
% Examples:
%
%    solve 1 3 2 6 5 4
% 1 2 1
%    solve 5 6 2 1 4 3
% 0 2
%    solve 6 5 4 1 2 3
% 0 1 2
%
% """

%
% Here is a GAP program for this problem.
% Note: It actually solves the opposite problem:
% Given a sequence, how to construct it, i.e. the 
% order of operations are reversed:
%
% Coding: the three operations (reverse) as cycle notation:
%
%   1 2 3 4 5 6      (1,4)(2,3)
%   4 3 2 1 5 6     
%
%   1 2 3 4 5 6      (2,5)(3,4)
%   1 5 4 3 2 6     
%
%   1 2 3 4 5 6      (3,6)(4,6)
%   1 2 6 5 4 3
%
%   Now the GAP code:
% 
%   gap> g:=Group([(1,4)(2,3), (2,5)(3,4), (3,6)(4,5)]);
%   Group([ (1,4)(2,3), (2,5)(3,4), (3,6)(4,5) ])
%   gap> Order(g);                                      
%   360
%   gap> a:=g.1; b:=g.2; c:=g.3;                        
%   (1,4)(2,3)
%   (2,5)(3,4)
%   (3,6)(4,5)
%   gap> StructureDescription(g);
%   "A6"
%   gap> ListPerm(a);
%   [ 4, 3, 2, 1 ]
%   gap> ListPerm(b);
%   [ 1, 5, 4, 3, 2 ]
%   gap> ListPerm(c);
%   [ 1, 2, 6, 5, 4, 3 ]
%
%   And the three problems:
%   gap> Factorization(g,PermList([1,3,2,6,5,4]));
%   x2*x3*x2
%
%   gap> Factorization(g,PermList([5,6,2,1,4,3]));
%   x1*x3
%
%   gap> Factorization(g,PermList([6,5,4,1,2,3]));
%   x1*x2*x3
%
%

% Note: This is a version of http://hakank.org/1d_rubiks_cube.mzn
% The difference is that the moves are represented as permutations
% and not procedures.


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: max_steps = 15;

int: n;
int: num_moves;
array[0..num_moves, 1..n] of int: moves;
array[1..n] of 1..n: data; % input sequence

% decision variables
% array[1..n] of var 1..n: data; % input sequence
array[1..max_steps, 1..n] of var 1..n: steps;
array[1..max_steps] of var 0..num_moves: ops;
var 1..max_steps: min_step;

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

% solve satisfy;
% solve :: minimize min_step;
% ann: var_select;
% ann: val_select;
% smallest/indomain_min 164863 failures
% 
% 
solve :: int_search(
        % [steps[i,j] | i in 1..max_steps, j in 1..n] 
        % [steps[max_steps-i+1,n-j+1] | i in 1..max_steps, j in 1..n] 
        ops
        , 
        smallest, % first_fail, 
        indomain_min, % indomain_min, 
        complete) 
    minimize min_step;
    % satisfy;



% general constraints
constraint
  % init
  forall(j in 1..n) (
     steps[1,j] = data[j]
  )
  /\
  ops[1] = 0

  /\
  forall(i in 1..max_steps) (
     alldifferent([steps[i,j] | j in 1..n]) % :: domain
     /\
     % don't repeat moves if ops > 0 (the nop move)
     (ops[i-1] > 0 -> ops[i] != ops[i-1]) 
  )
;


% the operations
%  * either reverse
%     1..4
%     2..5
%     3..6
%  * same as previous line
constraint
   % the move from steps[i-1,...] -> steps[i, ...]
   % the let version seems to be faster than using exists(m in 0..num_moves)
   % forall(i in 2..max_steps) (
   %   let { var 0..num_moves: m } in
   %   % exists(m in 0..num_moves) (
   %     forall(k in 1..n) ( steps[i-1,k] = moves[m, steps[i,k]] )
   %     /\
   %     ops[i] = m
   %   % )
   % )
   % /\
   forall(i in 2..max_steps) (
      let {
        var 0..num_moves: m
      } in
      permutation3([steps[i-1,k] | k in 1..n],[moves[m,k] | k in 1..n],[steps[i,k] | k in 1..n])  % :: domain
      % permutation3([steps[i,k] | k in 1..n],[moves[m,k] | k in 1..n],[steps[i-1,k] | k in 1..n])  % :: domain
      /\ 
      ops[i] = m
   )

   % /\ % there must be some sequence 1..n (the goal)
   % exists(i in 1..max_steps) (
   %    forall(j in 1..n) ( steps[i,j] = j )
   %    /\
   %    min_step = i
   %    /\ % and all the further steps must also be the solution
   %       % (symmetry breaking)
   %    forall(k in i+1..max_steps) (
   %      forall(j in 1..n) (steps[k,j] = j)
   %      /\ ops[k] = 0
   %    )
   % )

   /\ % this is often much faster than the exists approach
   forall(j in 1..n) ( steps[min_step,j] = j )

   /\ % Symmetry breaking: 
      % all the further entries after check_ix steps must also be the solution.
   forall(k in 2..max_steps) (
      k > min_step -> (forall(j in 1..n) (steps[k,j] = j) 
                       /\ ops[k] = 0)
   )

   /\ % this seems to be a good booster
   forall(i in 2..max_steps) (
      i < min_step -> ops[i] > 0
   )
;

output [
 "min_step: " ++ show(min_step) ++ "\n" ++
 "ops: " ++ show(ops) ++ "\n" 
] ++
[
  if i > 1 /\ fix(ops[i]) > 0 then
    if j = 1 then "\nstep(" ++ show_int(2, i) ++ "): " else " " endif ++
     show_int(2, steps[i,j])  
     ++ % show the op
     if j = n then " (op " ++ show_int(2, ops[i]) ++ ")" else "" endif
  else 
    ""
  endif
  | i in 1..max_steps, j in 1..n
]
 ++ ["\n"]
;


%
% data
%


% n = 6;
% num_moves = 3;
% % the legal moves: 1,2,3,4,5,6 -> <the move>
% % Note the 0 move (nop)
% moves = 
%  array2d(0..num_moves, 1..n,
%   [
%    1,2,3,4,5,6,    % move 0 (nop move)
%    4,3,2,1,    5,6, % move 1
%    1,  5,4,3,2,  6, % move 2
%    1,2,    6,5,4,3,    % move 3
%   ]);

% 
% data = [1,3,2,6,5,4];
% data = [5,6,2,1,4,3];
% data = [6,5,4,1,2,3];

% the two hardest problems, take 11 moves 
% There are 6 different solutions with 11 moves for these problems.
% (12 including the initial step).
% data = [6,3,5,2,4,1];   % GAP: x3*x1*x2*x1*x3*x2*x1*x2*x1*x3*x1
% data = [6,4,2,5,3,1];   % GAP: x1*x3*x2*x3*x2*x1*x3*x2*x3*x2*x1
% constraint min_step = 12;

% infeasible
% data = [6,5,4,3,2,1];


% n = 8;
% num_moves = 5;
% % Note the 0 move (nop)
% moves = 
%  array2d(0..num_moves, 1..n,
%   [
%    1,2,3,4,5,6,7,8,    % move 0 (nop move)
%    4,3,2,1,    5,6,7,8, % move 1
%    1,  5,4,3,2,  6,7,8, % move 2
%    1,2,    6,5,4,3,7,8, % move 3
%    1,2,3,  7,6,5,4,  8, % move 4
%    1,2,3,4,    8,7,6,5, % move 5
%   ]);

% initial data
% test with another length (8) with the same seq_length (4)
% The hardest is 10 moves (there are 156 with 10 moves according to GAP)
% data = [6,8,3,7,1,4,2,5]; % 8 steps

% These takes 10 steps
% cp1d(data, [2,4,1,7,5,3,8,6]) % GAP: x2*x3*x2*x4*x3*x5*x4*x1*x2*x1
% data = [8,7,6,3,2,5,4,1];  % x3*x1*x2*x3*x1*x4*x5*x1*x3*x1




% n = 10;
% num_moves = 7;
% % Note the 0 move (nop)
% moves = 
%  array2d(0..num_moves, 1..n,
%   [
%    1,2,3,4,5,6,7,8,9,10,     % move 0 (nop move)
%    4,3,2,1,    5,6,7,8,9,10, % move 1
%    1,  5,4,3,2,  6,7,8,9,10, % move 2
%    1,2,    6,5,4,3,7,8,9,10, % move 3
%    1,2,3,  7,6,5,4,  8,9,10, % move 4
%    1,2,3,4,  8,7,6,5,  9,10, % move 5
%    1,2,3,4,5,  9,8,7,6, 10,  % move 6
%    1,2,3,4,5,6, 10,9,8,7,    % move 7
%   ]);


n = 12;
num_moves = 9;
% Note the 0 move (nop)
moves = 
 array2d(0..num_moves, 1..n,
  [
   1,2,3,4,5,6,7,8,9,10,11,12,     % move 0 (nop move)
   4,3,2,1,    5,6,7,8,9,10,11,12, % move 1
   1,  5,4,3,2,  6,7,8,9,10,11,12, % move 2
   1,2,  6,5,4,3,  7,8,9,10,11,12, % move 3
   1,2,3,  7,6,5,4,  8,9,10,11,12, % move 4
   1,2,3,4,  8,7,6,5,  9,10,11,12, % move 5
   1,2,3,4,5,  9,8,7,6,  10,11,12, % move 6
   1,2,3,4,5,6, 10,9,8,7,   11,12, % move 7
   1,2,3,4,5,6,7, 11,10,9,8,   12, % move 8
   1,2,3,4,5,6,7,8, 12,11,10,9,    % move 9
  ]);


% Size 12
% Chuffed: smallest/indomain_min: 21.5s (59810 conflicts)  (using permutation3/3 and -toggle_vsids=true)
% Gecode/fz: smallest/indomain_min: 9.5s (62384 failures) (using permutation3/3)
% data = [7, 5, 11, 8, 9, 1, 10, 3, 4, 2, 6, 12];

% Size 12
% Chuffed: smallest/indomain_min: 11.3s (32059 conflicts)  (using permutation3/3 and -toggle_vsids=true)
% Gecode/fz: smallest/indomain_min: 4.9s (33524 failures)  (using permutation3/3)
data = [ 12, 2, 7, 3, 4, 11, 1, 10, 8, 9, 6, 5 ];


