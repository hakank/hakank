% 
% A league of their own (Enigma #1217) in MiniZinc.
% 
% https://enigmaticcode.wordpress.com/2015/07/18/enigma-1217-a-league-of-their-own/
% """
% From New Scientist #2372, 14th December 2002
%
% Four pubs recently played a round-robin football tournament, in which 
% each team played each of the others twice — home and away. George has 
% drawn up the final league table, using this grid.
%
%     [
%                    Played  Won   Drawn  Lost  Points
%                  -----------------------------------
%        Fagan's   |       |      |      |     |      |
%        George    |       |      |      |     |      |
%        Harlequin |       |      |      |     |      |
%        Inkerman  |       |      |      |     |      |
%                  -----------------------------------
%       
%     ]
% 
% With two points for a win and one for a draw, the teams finished, coincidentally, 
% in alphabetical order, as shown. George found further surprises:
% 
%   1. The four columns in the table — Won, Drawn, Lost, Points — each 
%      contained four different numbers;
%   2. Although Fagan’s won the tournament, the George won more games;
%   3. There were more away wins than home wins.
% 
%  Which matches were drawn? Identify each as “X v. Y”, naming the home team first.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 4;
int: home = 1;
int: away = 2;

int: win = 2;
int: draw = 1;
int: loss = 0;

int: won = 1;
int: drawn = 2;
int: lost = 3;
int: points = 4;

int: num_types = 4;

% decision variables
array[1..n,1..n,home..away] of var loss..win: games;

array[1..n, 1..num_types] of var 0..6*2: scores;
array[1..num_types] of string: score_types = ["Won   ",
                                              "Drawn ",
                                              "Lost  ",
                                              "Points"];

array[1..n] of string: teams = ["Fagan's  ",
                                "George   ",
                                "Harlequin",
                                "Inkerman "];

% solve satisfy;
solve :: int_search(array1d(games), first_fail, indomain_split, complete) satisfy;

constraint
  % restrict domains of all score types except points
  forall(t1 in 1..n, t in {won,drawn,lost}) (
    scores[t1,t] <= 6
  )
  /\
  % define the scores 
  forall(t1 in 1..n) (
     forall(k in home..away) ( games[t1,t1,k] = 0)
     /\ scores[t1,won]    = sum([games[t1,t2,k] = win  | t2 in 1..n, k in home..away])
     /\ scores[t1,drawn]  = sum([games[t1,t2,k] = draw | t2 in 1..n, k in home..away])
     /\ scores[t1,lost]   = sum([games[t1,t2,k] = loss | t2 in 1..n, k in home..away where t1 != t2])
     /\ scores[t1,points] = win*scores[t1,won]+draw*scores[t1,drawn]
  )
  /\ % symmetry of the different scores
  forall(t1 in 1..n, t2 in 1..n, k in home..away where t1 != t2) (
    (games[t1,t2,k] = win  <-> games[t2,t1,k] = loss) /\
    (games[t1,t2,k] = draw <-> games[t2,t1,k] = draw)
  )

  % """... the teams finished, coincidentally,  in alphabetical order"""
  /\ decreasing([scores[t1,points] | t1 in 1..n]) 

  % """1. The four columns in the table — Won, Drawn, Lost, Points — each 
  %      contained four different numbers;
  % """
  /\
  forall(t in {won,drawn,lost,points}) (
    all_different([scores[t1,t]    | t1 in 1..n]) 
  )

  % """2. Although Fagan’s won the tournament, the George won more games;"""
  /\ scores[2,won] > scores[1,won]

  % """3. There were more away wins than home wins."""
  /\ sum([ games[t1,t2,away] = win | t1,t2 in 1..n]) > sum([ games[t1,t2,home] = win | t1,t2 in 1..n])


  % redundant constraints (slighly faster)
  /\ sum([games[t1,t2,home] | t1,t2 in 1..n]) = 2*6
  /\ sum([games[t1,t2,away] | t1,t2 in 1..n]) = 2*6
  /\ sum([scores[t1,points] | t1 in 1..n]) = 4*6
;

output 
[
  "           Won   Drawn Lost  Points",
]
++
[
   if t = won then "\n" ++ show(teams[team]) ++ ": " else "" endif ++
     show(scores[team,t]) ++ "     "
   | team in 1..n, t in {won,drawn,lost,points}
]
++ [ "\nHome:" ] ++
[
  if t2 = 1 then "\n" else " " endif ++
    show(games[t1,t2,1]) 
  | t1,t2 in 1..n
]
++ [ "\nAway:" ] ++
[
  if t2 = 1 then "\n" else " " endif ++
    show(games[t1,t2,2]) 
  | t1,t2 in 1..n
]
++
[ "\nDraws:\n"]
++
[
  if fix(games[t1,t2,1]) = 1 then
     show(teams[t1]) ++ " vs. " ++ show(teams[t2]) ++ "\n"
  else "" endif
  | t1,t2 in 1..n where t1 < t2
]
++
[
  if fix(games[t1,t2,2]) = 1 then
     show(teams[t2]) ++ " vs. " ++ show(teams[t1]) ++ "\n"
  else "" endif
  | t1,t2 in 1..n where t1 < t2
]
;

