%
% Minesweeper in MiniZinc.
%
% This is a general model
% 

% From gecode/examples/minesweeper.cc:
% """
% A specification is a square matrix of characters. Alphanumeric characters represent
% the number of mines adjacent to that field. Dots represent fields with an unknown number
% of mines adjacent to it (or an actual mine).
% """
% 
% E.g.
%      "..2.3."
%      "2....."
%      "..24.3"
%      "1.34.."
%      ".....3"
%      ".3.3.."
% """
% 
% Also see 
%  
% http://www.janko.at/Raetsel/Minesweeper/index.htm
%  (the first 10 examples are from)

% http://en.wikipedia.org/wiki/Minesweeper_(computer_game)
%
% Ian Stewart on Minesweeper: http://www.claymath.org/Popular_Lectures/Minesweeper/

% Richard Kaye's Minesweeper Pages
% http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.htm
% Some Minesweeper Configurations
% http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.pdf


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: r; % rows
int: c; % column
int: X = -1; % the unknowns

% encoding: -1 for unknown, >= 0 for number of mines in the neighbourhood
array[1..r, 1..c] of -1..8: game;
array[1..r, 1..c] of var 0..1: mines;

constraint
    forall(i in 1..r, j in 1..c) (
      (
       (game[i,j] >= 0 )
        ->
        game[i,j]  = sum(a,b in {-1,0,1} where 
            i+a > 0  /\ j+b >  0 /\
            i+a <= r /\ j+b <= c
         )
           (mines[i+a,j+b])
      )
      /\
      (game[i,j] > X -> mines[i,j] = 0) 
      /\
      (game[i,j] = X <- mines[i,j] = 1) 
    )
;

solve :: int_search([mines[i,j] | i in 1..r, j in 1..c], first_fail, indomain_min, complete) satisfy;


output 
[ "\nmines:" ] ++
% [
%   if j = 1 then "\n" else " " endif ++
%     show(mines[i,j])
%   | i in 1..r,j in 1..c
% ] ++ ["\n"]
[
  if j = 1 then "\n" else "" endif ++
    if fix(mines[i,j]) = 1 then
      "*"
    else 
      "."
    endif
  | i in 1..r,j in 1..c
] ++ ["\n"]

;

