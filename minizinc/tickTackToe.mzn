% 
% Tic Tac Toe in MiniZinc.
% 
% From the Minion / Tailor model tickTackToe.eprime
% (modelled by Andrea Rendl)
% 
% """
% The Game Tic Tac Toe
%
%   o| o| x
%  --------
%    | x|
%  --------
%   x| o|
%
% Two player game where each player consecutevly
% marks a space with a circle o  or and x.
%
% The black player starts and we want black to win.
% """
%
% For more about Minion/Tailor, see
% http://www.cs.st-andrews.ac.uk/~andrea/tailor/index.html
% 
%
%
% Note: I added some constraints. See below.
%
%
% The first solution by Gecode/fz is:
%
% Start position
% 2 2 2
% 2 2 2
% 2 2 2
%
% Player 0 place at [1,1]
% 0 2 2
% 2 2 2
% 2 2 2
% 
% Player 1 place at [1,2]
% 0 1 2
% 2 2 2
% 2 2 2
% 
% 0 1 0
% 2 2 2
% 2 2 2
% 
% 0 1 0
% 2 1 2
% 2 2 2
% 
% 0 1 0
% 0 1 2
% 2 2 2
% 
% Here player 1 wins (in 6 steps)
% 0 1 0
% 0 1 2
% 2 1 2
% 
% Since we require 7 steps 0 continues
% 0 1 0
% 0 1 0
% 2 1 2
%
%
% There are 14544 solutions for the 7 steps problem, found by Gecode/fz in
% 1 min 22 seconds (238869881 propagations).
% 
% There are 5712 solutions for the 6 steps problem found in 30 seconds
% by Gecode/fz (84931979 propagations).
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

%  in how many steps do we want to win the game
int: steps;
int: EMPTY = 2;
int: WHITE = 1;
int: BLACK = 0;

0..2: WINNER; % black or white
0..2: LOOSER = (WINNER + 1); % 2 % the opposite of WINNER

set of int: STEPS0 = 0..steps;
set of int: STEPS1 = 1..steps;
set of int: LENGTH = 1..3;
% either empty, white or black
set of int: FIELD_STATUS = 0..2;

array[STEPS0, LENGTH, LENGTH] of var FIELD_STATUS: tickTackToe;


% solve satisfy;
solve :: int_search([tickTackToe[i,j,k] | i in STEPS0, j, k in LENGTH], 
        first_fail, indomain_min, complete) satisfy;

constraint

   % initial state
   forall(i,j in LENGTH) (
     tickTackToe[0,i,j] = EMPTY
   )
   /\

   % either nothing changes
   (
     (
        forall(step in STEPS1) (
           ( sum(row, col in LENGTH) (tickTackToe[step-1,row,col] ) ) 
            =
           ( sum(row, col in LENGTH) (tickTackToe[step,row,col] ) )
        )
      )
    \/
    % or there is just one change
     ( 
       forall(step in STEPS1) (
         ( sum(row, col in LENGTH) ( tickTackToe[step-1,row,col] ) 
          - ((step mod 2) + 1 ) )
          =
         ( sum(row, col in LENGTH) ( tickTackToe[step,row,col] ) )
  
       )
     )
   )

   /\

   % if a field is empty, it may be occupied in the next step
   forall(step in STEPS1) (
      ( 
        exists(row, col in LENGTH) (
          % there is an empty field that
          (tickTackToe[step-1,row,col] = EMPTY ) 
          /\
          % the player whose turn it is occupies
          (tickTackToe[step,row,col] = (step mod 2)+1 ) % black has even moves, white has odd moves
          /\ 
          % and no other field changes
          forall(i,j in LENGTH) (
            ((i != row) /\ (j!=col)) ->
               (tickTackToe[step,i,j] = tickTackToe[step-1,i,j])
          )
       )
     )
   )

   /\

   % if a player already took the field, then it belongs
   % to the player forever
   forall(step in STEPS1) (
     forall(row, col in LENGTH) (
       (tickTackToe[step-1,row,col] != EMPTY ) ->
          (tickTackToe[step,row,col] = tickTackToe[step-1,row,col])
     )
   )

   /\
 
   % goal state: WINNER wins
   % three in a row
   (
     (
        exists(row in LENGTH) (
          forall(col in LENGTH) (
             (tickTackToe[steps, row,col] = WINNER)
           )
       )
       \/
       % three in a column
       exists(col in LENGTH) (
         forall(row in LENGTH) (
           tickTackToe[steps, row,col] = WINNER
         )
       )
       \/
       % three in the main diagonal
       forall(i in LENGTH) (
          tickTackToe[steps,i,i] = WINNER
       )
       \/
       % three in the other diagonal
       ( 
         tickTackToe[steps,1,3] = WINNER /\ 
         tickTackToe[steps,2,2] = WINNER /\ 
         tickTackToe[steps,3,1] = WINNER 
       )

    ) % end the OR clause

  )

  % hakank: count the number of 0's and 1's. There must be the same
  %         amount of numbers, or +/- 1.
  /\
  abs(sum(j,k in LENGTH) ( bool2int(tickTackToe[steps, j,k]=WINNER)) -
  sum(j,k in LENGTH) ( bool2int(tickTackToe[steps, j,k]=LOOSER))) <= 1
  
  /\ % hakank: sanity check: the number of 0's and 1's must be == steps
  sum(j,k in LENGTH) ( bool2int(tickTackToe[steps, j,k] < 2)) = steps

  /\

   % and LOOSER has not won
   % no three in a row
   (
     forall(row in LENGTH) (
       exists(col in LENGTH) (
         (tickTackToe[steps, row,col] != LOOSER)
       )
     )
     \/
     % no three in a column
     forall(col in LENGTH) (
       exists(row in LENGTH) ( 
         (tickTackToe[steps, row,col] != LOOSER)
       )
     )
     \/
     % three in the main diagonal
     exists(i in LENGTH) (
       tickTackToe[steps,i,i] != LOOSER
     )
     \/
     % three in the other diagonal
     ( 
      tickTackToe[steps,1,3] != LOOSER \/ 
      tickTackToe[steps,2,2] != LOOSER \/ 
      tickTackToe[steps,3,1] != LOOSER
     )
  )
;


output [
   if i = 0 /\ j = 1 /\ k = 1 then "\n\nGame:" else "" endif ++
   if j = 1 /\ k = 1 then "\n" else "" endif ++
   if k = 1 then "\n" else " " endif ++
     show(tickTackToe[i,j,k]) 
   | i in STEPS0, j, k in LENGTH
] ++ ["\n"]
;

% from tickTackToe7.param
steps = 7;
WINNER = 1; % 0:black
            % 1:white
