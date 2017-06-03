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
% Chuffed seems to like these parameters:
%    -mdd=true -vsids=true -alldiff_cheat=false


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 8;
int: seq_length = 4; % 4; % length of each reverse sequence
int: max_steps = 15;
int: m = n-seq_length+1;

array[1..n] of var 1..n: data; % input sequence
array[1..max_steps, 1..n] of var 1..n: steps;
array[1..max_steps] of var 0..m: ops;
var 1..max_steps: min_step;

%
% reverse: y is x where positions from..to are reversed
%
predicate reverse(array[int] of var int: x, 
                  array[int] of var int: y, 
                  int: from, 
                  int: to) =
   let { 
     int: n = length(x)
   }
   in
    % do the segment reverse
    forall(i in from..to) ( 
         y[to-i+from] = x[i] 
      )
    /\ % ensure that all untouched positions are untouched
    forall(i in 1..n) (
       if i < from \/ i > to then
           y[i] = x[i]
       else
          true
       endif
    )
;

%
% same: x is the same sequence as y
%
predicate same(array[int] of var int: x, array[int] of var int: y) =
   let { 
     int: n = length(x)
   }
   in
   forall(i in 1..n) ( y[i] = x[i] )
;


% solve satisfy;
% solve :: minimize min_step;
solve :: int_search(
        [steps[i,j] | i in 1..max_steps, j in 1..n], 
        first_fail, 
        indomain_min, 
        complete) 
    minimize min_step;
    % satisfy;


% initial data
constraint
  % 
  % data = [1,3,2,6,5,4]
  % data = [5,6,2,1,4,3]
  % data =  [6,5,4,1,2,3]

  % the two hardest problems, take 11 moves 
  % (12 including the initial step).
  % data =  [6,3,5,2,4,1]   % GAP: x3*x1*x2*x1*x3*x2*x1*x2*x1*x3*x1
  % data =  [6,4,2,5,3,1]   % GAP: x1*x3*x2*x3*x2*x1*x3*x2*x3*x2*x1

  % infeasible
  % data =  [6,5,4,3,2,1]

  % test with another length (8) with the same seq_length (4)
  % The hardest is 10 moves (there are 156 with 10 moves according to GAP)
  data =  [6,8,3,7,1,4,2,5] % 8 steps

  % These takes 10 steps
  % data =  [2,4,1,7,5,3,8,6] % GAP: x2*x3*x2*x4*x3*x5*x4*x1*x2*x1
  % data =  [8,7,6,3,2,5,4,1]  % x3*x1*x2*x3*x1*x4*x5*x1*x3*x1

  % Size 12
  % Chuffed took 1:24 minutes (min_step: 13)
  % data =  [7, 5, 11, 8, 9, 1, 10, 3, 4, 2, 6, 12]

  % Chuffed took 3:27 minutes to solve this (min_step: 13)
  % data =  [ 12, 2, 7, 3, 4, 11, 1, 10, 8, 9, 6, 5 ]



;

% general constraints
constraint
  trace("n: " ++ show(n) ++ " seq_length: " ++ show(seq_length) ++ " m: " ++ show(m) ++ "\n", 1=1)
  /\
  forall(j in 1..n) (
     steps[1,j] = data[j]
  )
  /\
  ops[1] = 0
  /\ 
  forall(i in 2..max_steps) (
     alldifferent([steps[i,j] | j in 1..n])
     /\
     % don't repeat reverses (i.e. if ops > 0)
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

   forall(i in 2..max_steps) (
      (  % same as previous step
         same([steps[i,j] | j in 1..n], [steps[i-1,j] | j in 1..n])
         /\
         ops[i] = 0
      )
      \/
      % reverse from 1, 2 or 3,...
      exists(k in 1..m) (
            reverse([steps[i,j] | j in 1..n], 
                    [steps[i-1,j] | j in 1..n], 
                     k, 
                     k+seq_length-1)
           /\
           ops[i] = k
      )
   )

   /\ % there must be some sequence 1..n (the goal)
   exists(i in 1..max_steps) (
      forall(j in 1..n) (
         steps[i,j] = j
      )
      /\
      min_step = i
      /\ % and all the further steps should be "same" 
         % (symmetry breaking)
      forall(k in i+1..max_steps) (
        same([steps[k,j] | j in 1..n], [steps[k-1,j] | j in 1..n])
      )
   )
   /\ % this seems to be a good booster (or perhaps not...)
   forall(i in 2..max_steps) (
      i < min_step -> ops[i] > 0
   )

;

output [
 "min_step: " ++ show(min_step) ++ "\n" ++
 "ops: " ++ show(ops) ++ "\n" 
] ++
[
  if j = 1 then "\n" else " " endif ++
   show(steps[i,j])  
   ++ % show the op
   if j = n then " (op " ++ show(ops[i]) ++ ")" else "" endif
  | i in 1..max_steps, j in 1..n
]
 ++ ["\n"]
;

