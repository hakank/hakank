% 
% Seating arrangement problem in MiniZinc.
% 
% Problem statement from
% From Texas Action Group (mailing list)
% "Seating Arrangements: Solutions"
% http://www.cs.utexas.edu/users/vl/tag/seating_problem
% """
% You are organizing a New Year's Eve party.  There will be N tables
% in the room, with M chairs around each table.  You need to select a
% table for each of the guests, who are assigned numbers from 1 to MN,
% so that two conditions are satisfied.  First, some guests like each
% other and want to sit together; accordingly, you are given a set A of
% two-element subsets of {1,...,MN}, and, for every {i,j} in A, guests i
% and j should be assigned the same table.  Second, some guests dislike
% each other and want to sit at different tables; accordingly, you are
% given a set B of two-element subsets of {1,...,MN}, and, for every
% {i,j} in B, guests i and j should be assigned different tables.  Your
% program should find such a seating arrangement or determine that it
% is impossible.
% """
%
% Problem instances (Answer Set Programming) from 
% "Seating Arrangements: Solutions"
% http://www.cs.utexas.edu/users/vl/tag/seating_solutions
%
% Also, see the discussions about the problem and solutions 
% from an Answer Set Programming point of view.
% http://www.cs.utexas.edu/users/vl/tag/seating_discussion
%
%
% Problem instances for the MiniZinc model:
% - http://www.hakank.org/minizinc/seating_arrangements1.dzn
% - http://www.hakank.org/minizinc/seating_arrangements2.dzn
% - http://www.hakank.org/minizinc/seating_arrangements3.dzn
% 
%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n; % number of tables
int: m; % number of chair at each table
int: k = n*m; % number of persons

int: num_likes;
array[1..num_likes, 1..2] of int: likes;

int: num_dislikes;
array[1..num_dislikes, 1..2] of int: dislikes;


% decision variables

% which table is assigned for the persons
array[1..k] of var 1..n: x;
array[1..n] of var set of 1..k: tables;

% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   % channel between tables and x
   int_set_channel(x, tables)
; 

constraint

   forall(t in 1..n) (
      card(tables[t]) = m

      %% channel between tables and x (use int_set_channel instead)
      % /\
      % forall(j in 1..k) (
      %   x[j] = t <-> j in tables[t]
      % )
   )

   /\ % ensure that people who like each other
      % is at the same table
   forall(i in 1..num_likes) (
      x[likes[i,1]] = x[likes[i,2]]
   )

   /\ % ensure that people who dislike each other
      % is not at the same table
   forall(i in 1..num_dislikes) (
      x[dislikes[i,1]] != x[dislikes[i,2]]
   )

   /\ % symmetry breaking
   x[1] = 1

   /\ % symmetry breaking.
      % However, Gecode/fz don't like this
   decreasing_set(tables)

;


% Problem instance from
% http://www.cs.utexas.edu/users/vl/tag/seating_solutions
% "PROGRAM 11, for smodels,  By Chitta Baral (chitta@asu.edu)"
% n = 3;
% m = 3;

% num_likes = 3;
% likes = array2d(1..num_likes, 1..2,
%         [
%         1,2,
%         7,4,
%         6,8
%         ]);

% num_dislikes = 4;
% dislikes = array2d(1..num_dislikes, 1..2,
%         [
%         2,3,
%         3,4,
%         5,6,
%         9,4
%     ]);


output [
"x: " ++ show(x) ++ "\n"++
"tables: " ++ show(tables) ++ "\n"
];
