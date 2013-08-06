% 
% Virtual Chess Tournament in MiniZinc.
% 
% From Jacob Feldman's OpenRules Blog
% http://openrules.wordpress.com/2012/04/27/fisher-vs-kasparov-vs-karpov/
% Virtual Chess Tournament
% Three world champions Fisher, Kasparov, and Karpov played in a virtual chess tournament. 
% Each player played 7 games against two other opponents. Each player received 2 points 
% for a victory, 1 for a draw, and 0 for a loss. We know that Kasparov, known as the 
% most aggressive player, won the most games. Karpov, known as the best defensive player, 
% lost the least games. And Fisher, of course, won the tournament.
% 
% Questions
% Is it really possible? Does this problem have a solution? If yes, what is the final 
% score? Are there other possible solutions? 
% """
%
%  
% Note: The approach in this model is a bit different to the one in Jacob's blog post.
%       Though with the symmetry breaking we also got two different solutions, where
%       the only difference is that match 18 (Kasparov vs Karpov) is either a 
%       draw (solution 1) or that Karpov won (solution 2).
%
%       The order of the player arrays are 
%           [Fisher, Kasparov, Karpov]
% 
%       First solution:
%       x         : [1, 5, 7, 1, 5, 7, 1, 5, 8, 1, 5, 8, 3, 5, 8, 3, 5, 8, 3, 5, 9]
%       total     : [15, 14, 13]
%       num_wins  : [4, 5, 1]
%       num_losses: [3, 5, 2]
%       winners   : [1, 0, 2, 1, 0, 2, 1, 0, 0, 1, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 3]
%       losers    : [2, 0, 3, 2, 0, 3, 2, 0, 0, 2, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 2]
%       num_draws_total : 11
%
% 
%       Second solution:
%       x         : [1, 5, 7, 1, 5, 7, 1, 5, 8, 1, 5, 8, 3, 5, 8, 3, 5, 9, 3, 5, 9]
%       total     : [15, 13, 14]
%       num_wins  : [4, 5, 2]
%       num_losses: [3, 6, 2]
%       winners   : [1, 0, 2, 1, 0, 2, 1, 0, 0, 1, 0, 0, 2, 0, 0, 2, 0, 3, 2, 0, 3]
%       losers    : [2, 0, 3, 2, 0, 3, 2, 0, 0, 2, 0, 0, 1, 0, 0, 1, 0, 2, 1, 0, 2]
%       num_draws_total : 10
%
%
% Almost all MiniZinc solvers found these two solutions (and no other) in a jiffy, 
% i.e. < 1s. Without the symmetry breaking, there's a lot more solutions 
% (and longer solving time).
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
int: n = 7; % number of games
int: num_players = 3;
int: n2 = n*num_players;

% points
int: loss = 0;
int: draw = 1;
int: win = 2;

% Winner/Loser
int: Draw = 0;
int: Fisher = 1;
int: Kasparov = 2;
int: Karpov = 3;

% Players
array[0..3] of string: s = array1d(0..3, ["Draw", "Fisher","Kasparov","Karpov"]);

% Matches
array[1..3] of string: s2 = [
  "Fisher vs Kasparov",
  "Fisher vs Karpov",
  "Kasparov vs Karpov",
];

% 
% There are 3 types of matches (P1 vs P2)
% which we defined as 1,2,3 (1+((i-1) mod 3))
% 
%  1:  Fisher vs Kasparov
%  2:  Fisher vs Karpov
%  3:  Kasparov vs Karpov
% each with 3 different outcomes
%    P1 wins
%    Draw
%    P2 wins
%
% Here's the number of points each player get for the diffent
% outcome.
%
array[1..9, 1..3] of int: M = array2d(1..9, 1..3,
[
2,0,0,  % Match type 1 Fisher vs Kasparov: Fisher wins
1,1,0,  % Match type 1 Fisher vs Kasparov: Draw 
0,2,0,  % Match type 1 Fisher vs Kasparov: Kasparov wins

2,0,0,  % Match type 2 Fisher vs Karpov  : Fisher wins
1,0,1,  % Match type 2 Fisher vs Karpov  : Draw
0,0,2,  % Match type 2 Fisher vs Karpov  : Karpov wins

0,2,0,  % Match type 3 Kasparov vs Karpov: Kasparov wins
0,1,1,  % Match type 3 Kasparov vs Karpov: Draw
0,0,2,  % Match type 3 Kasparov vs Karpov: Karpov wins

]);

% Which are the possible outcome from each type of match
array[1..3] of set of int: valid_outcome = 
[
  {1,2,3}, % match type 1 
  {4,5,6}, % match type 2 
  {7,8,9}  % match type 3
];

% Winners of each match type
array[1..9] of int: W =
[
  Fisher,   Draw, Kasparov,
  Fisher,   Draw, Karpov,
  Kasparov, Draw, Karpov
];

% Losers of each match type
array[1..9] of int: L =
[
  Kasparov, Draw, Fisher,
  Karpov,   Draw, Fisher,
  Karpov,   Draw, Kasparov
];


% decision variables
array[1..n2] of var 1..9: x; % outcome of each match
array[1..num_players] of var 0..n2*win: total; % total point for each player
array[1..n2] of var 0..num_players: winners; % 0 is draw
array[1..n2] of var 0..num_players: losers; % 0 is a draw

array[1..num_players] of var 0..n: num_wins;
array[1..num_players] of var 0..n: num_losses;

var 0..n2: num_draws_total = sum([bool2int(winners[i] = 0) | i in 1..n2]);

% solve satisfy;
solve :: int_search(
        x,
        first_fail, 
        indomain_min, 
        complete) 
    satisfy;

constraint

   forall(i in 0..n2-1) (
      x[i+1] in valid_outcome[1+(i mod 3)]
   )

   /\
   forall(i in 1..n2) (
       winners[i] = W[x[i]] /\
       losers[i]  = L[x[i]] 
   )

   /\
   forall(p in 1..num_players) (
     total[p] = sum([M[x[i],p] | i in 1..n2]) 
     /\ num_wins[p] = sum([bool2int(winners[i] = p) | i in 1..n2])
     /\ num_losses[p] = sum([bool2int(losers[i] = p) | i in 1..n2])
   )

   % symmetry breaking: order the outcomes for each match type
   % /\
   % x[1] <= x[4] /\
   % x[2] <= x[5] /\
   % x[3] <= x[6]
   /\ % more general symmetry breaking
   forall(i in 1..n2-3) (
      x[i] <= x[i+3]
   )
;

%
% The three stated constraints.
% 

% We know that Kasparov, known as the most aggressive player, won the most games. 
constraint
  num_wins[Kasparov] > num_wins[Fisher] /\
  num_wins[Kasparov] > num_wins[Karpov]
;

% Karpov, known as the best defensive player, lost the least games. 
constraint
  num_losses[Karpov] < num_losses[Fisher] /\
  num_losses[Karpov] < num_losses[Kasparov]
;


% And Fisher, of course, won the tournament.
constraint
  total[Fisher] > total[Kasparov] /\
  total[Fisher] > total[Karpov]
;


output 
[
  "x         : " ++ show(x) ++ "\n" ++
  "total     : " ++ show(total) ++ "\n" ++
  "num_wins  : " ++ show(num_wins) ++ "\n" ++
  "num_losses: " ++ show(num_losses) ++ "\n" ++ 
  "winners   : " ++ show(winners) ++ "\n" ++ 
  "losers    : " ++ show(losers) ++ "\n" 
  ++ "num_draws_total : " ++ show(num_draws_total) ++ "\n"
]
++
[
  let {
    int: i2 = 1+i,
    int: m = 1+(i mod 3),
    int: r = fix(x[i+1])
  } in
  show_int(2, i2) ++ ": " ++ show(s2[m]) ++ " Winner/draw: " ++ 
            show(s[fix(winners[i2])]) ++ " (" ++ show(winners[i2]) ++ ")\n"
  | i in 0..n2-1
]
;

