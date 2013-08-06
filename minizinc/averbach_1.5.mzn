% 
% Nim in MiniZinc.
%
% Problem 1.5 from 
% Averbach & Chein "Problem Solving Through Recreational Mathematics".
% """
% Two players, A and B, take turns in the following game.
% There is a pile of six matchsticks. At a turn, a player must take
% one or two sticks from the remaining pile. The player who takes
% the last stick wins.
% Player A makes the first move and each player always make the
% best possible move.
% Who wins the game?
% """
% 
% This model generates all valid moves for a general Nim game.
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 


int: n; % number of matchsticks
int: max_num_to_take;
int: num_players;

% the move
array[1..n] of var 0..max_num_to_take: x;
% which player of the move
array[1..n] of 1..num_players: who = [ 1+(i mod num_players) | i in 0..n-1] ;

array[1..n] of var 0..num_players: winner; 
var 0..n: z =  sum(i in 1..n) (x[i]); % decision variable


array[1..n] of var 0..n: z_cumsum; % cumulative sum of moves

% minimize number of moves (maximize number of 0's?)
solve :: int_search(x, first_fail, indomain_min, complete)  satisfy;
% solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete)  maximize sum(i in 1..n) (bool2int(winner[i] = 1));

constraint
 forall(i in 1..n) (
    z < n ->
    (
      who[i] = 1+((i-1) mod num_players)
      /\
      x[i] in 1..max_num_to_take 
    )
 )
 /\ % no 0 before any real move
 forall(i in 1..n) (
    x[i] > 0 <->
    not exists(j in 1..i) ( x[j] = 0 )
 )
 /\ % calculate cumulative sum of moves, and announce the winner
 forall(i in 1..n) (
   z_cumsum[i] = sum(j in 1..i) ( x[j] )
   /\
   (z_cumsum[i] = n <-> winner[i] = who[i])
   /\
   (z_cumsum[i] != n <-> winner[i] = 0)
 )
 /\
 z <= n

;

output [
  "x       : ", show(x), "\n",
  "who     : ", show(who), "\n",
  "winner  : ", show(winner), "\n",
  "z_cumsum: ", show(z_cumsum), "\n",
];

%
% data
%
num_players = 2;
n = 6;
max_num_to_take = 2;