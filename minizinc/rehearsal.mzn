% 
% Scheduling a Rehearsal in MiniZinc.
% 
% From Barbara M. Smith: 
% "Constraint Programming in Practice: Scheduling a Rehearsal"
% http://www.dcs.st-and.ac.uk/~apes/reports/apes-67-2003.pdf
% """
% A concert is to consist of nine pieces of music of different durations 
% each involving a different combination of the five members of the orchestra. 
% Players can arrive at rehearsals immediately before the first piece in which 
% they are involved and depart immediately after the last piece in which 
% they are involved. The problem is to devise an order in which the pieces 
% can be rehearsed so as to minimize the total time that players are waiting 
% to play, i.e. the total time when players are present but not currently 
% playing. In the table below, 1 means that the player is required for 
% the corresponding piece, 0 otherwise. The duration (i.e. rehearsal time) 
% is in some unspecified time units.
%
%    Piece       1    2   3    4    5  6    7   8    9
%    Player 1    1    1   0    1    0  1    1   0    1
%    Player 2    1    1   0    1    1  1    0   1    0
%    Player 3    1    1   0    0    0  0    1   1    0
%    Player 4    1    0   0    0    1  1    0   0    1
%    Player 5    0    0   1    0    1  1    1   1    0
%    Duration    2    4   1    3    3  2    5   7    6
%
% For example, if the nine pieces were rehearsed in numerical order as 
% given above, then the total waiting time would be:
%       Player 1: 1+3+7=11
%       Player 2: 1+5=6
%       Player 3: 1+3+3+2=9
%       Player 4: 4+1+3+5+7=20
%       Player 5: 3
% giving a total of 49 units. The optimal sequence, as we shall see, 
% is much better than this.
%
% ...
% 
% The minimum waiting time for the rehearsal problem is 17 time units, and 
% an optimal sequence is 3, 8, 2, 7, 1, 6, 5, 4, 9.
%
% """

%
% The data above is in 
%   http://www.hakank.org/minizinc/rehearsal_smith.dzn
%

% Here are all optimal sequences for Barbara M. Smith's problem
% (total_waiting_time: 17)
% 
% order: [9, 4, 6, 5, 1, 7, 2, 8, 3]
% waiting_time: [3, 5, 0, 3, 6]
% total_waiting_time: 17
% ----------
% order: [9, 4, 6, 5, 1, 2, 7, 8, 3]
% waiting_time: [3, 5, 0, 3, 6]
% total_waiting_time: 17
% ----------
% order: [9, 4, 5, 6, 1, 7, 2, 8, 3]
% waiting_time: [3, 5, 0, 3, 6]
% total_waiting_time: 17
% ----------
% order: [9, 4, 5, 6, 1, 2, 7, 8, 3]
% waiting_time: [3, 5, 0, 3, 6]
% total_waiting_time: 17
% ----------
% order: [3, 8, 7, 2, 1, 6, 5, 4, 9]
% waiting_time: [3, 5, 0, 3, 6]
% total_waiting_time: 17
% ----------
% order: [3, 8, 7, 2, 1, 5, 6, 4, 9]
% waiting_time: [3, 5, 0, 3, 6]
% total_waiting_time: 17
% ----------
% order: [3, 8, 2, 7, 1, 6, 5, 4, 9]
% waiting_time: [3, 5, 0, 3, 6]
% total_waiting_time: 17
% ----------
% order: [3, 8, 2, 7, 1, 5, 6, 4, 9]
% waiting_time: [3, 5, 0, 3, 6]
% total_waiting_time: 17
% ----------
%
% Note that all waiting times are the same for 
% all sequences, i.e. player 1 always wait 3 units, etc.
%
% With symmetry breaking rule that order[1] < order[num_pieces] 
% there are 4 solutions where piece 2 and 7 can change place and 
% 5 and 6 can change place.
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 


int: num_pieces;
int: num_players;
array[1..num_pieces] of int: duration;
array[1..num_players, 1..num_pieces] of 0..1: rehearsal;


%
% Decision variables
%
array[1..num_pieces] of var 1..num_pieces: rehearsal_order;
array[1..num_players] of var 0..sum(duration): waiting_time; % waiting time for players
array[1..num_players] of var 1..num_pieces: p_from; % first rehearsal
array[1..num_players] of var 1..num_pieces: p_to;   % last rehearsal
var 0..sum(duration): total_waiting_time = sum(waiting_time); % objective

solve :: int_search(
         rehearsal_order % ++ waiting_time% ++ p_from ++ p_to ++ [total_waiting_time]
         , 
         first_fail, % occurrence, % max_regret, % first_fail, 
         indomain_max, % indomain_max, 
         complete) 
     minimize total_waiting_time;
     % satisfy;

% solve :: labelling_ff minimize total_waiting_time;

constraint
  all_different(rehearsal_order) :: domain
  /\

  % This solution is my own without glancing at Smith's models...
  forall(p in 1..num_players) (
     % This versions is much faster than using exists (see below)
     % fix the range from..to, i.e. don't count all that start with 0 
     % or ends with 0.
     % This means that we collect the rehearsals with many 0 at the ends
     %
     p_from[p] < p_to[p]
     /\
     % skipping rehearsal at start (don't come yet)
     forall(i in 1..num_pieces) (
        i < p_from[p] -> (rehearsal[p, rehearsal_order[i]] = 0)
     )
     /\
     % skipping rehearsal at end (go home after last rehearsal)
     forall(i in 1..num_pieces) (
        i > p_to[p] -> (rehearsal[p, rehearsal_order[i]] = 0)
     )
     /\ % and now: count the waiting time for from..to
     waiting_time[p] = 
         sum(i in 1..num_pieces) (
              duration[rehearsal_order[i]] * bool2int(
                                             i >= p_from[p] /\ i <= p_to[p] 
                                             /\
                                             rehearsal[p,rehearsal_order[i]] = 0
                                )
     ) 

%      % alternative solution with exists. 
%      %  More elegant (= declarative) in my book but slower.
%      exists(from, to in 1..num_pieces) ( 
%         % skipping rehearsal at start (don't come yet)
%         forall(i in 1..from-1) (
%            rehearsal[p, rehearsal_order[i]] = 0
%         )
%         /\
%         % skipping rehearsal at end (go home after last rehearsal)
%         forall(i in to+1..num_pieces) (
%            rehearsal[p, rehearsal_order[i]] = 0
%         )
%         /\ % and now: count the waiting time for from..to
%         waiting_time[p] = 
%             sum(i in from..to) (
%                  duration[rehearsal_order[i]]*
%                                  bool2int(
%                                       rehearsal[p,rehearsal_order[i]] = 0
%                                   )
%          ) 
%      )


  )

  /\ % symmetry breaking
  rehearsal_order[1] < rehearsal_order[num_pieces]

  % for all solutions
  % /\ total_waiting_time = 17
;


%
% data
%
%
% This is the problem from Barbara M. Smith's Rehearsal paper cited above:
% (see rehearsal_smith.dta)
% num_pieces = 9;
% num_players = 5;
% duration = [2, 4, 1, 3, 3, 2, 5, 7, 6];
% rehearsal = array2d(1..num_players, 1..num_pieces, 
%     [
%      1,1,0,1,0,1,1,0,1,
%      1,1,0,1,1,1,0,1,0,
%      1,1,0,0,0,0,1,1,0,
%      1,0,0,0,1,1,0,0,1,
%      0,0,1,0,1,1,1,1,0
%   ]);

%
% This is the problem from the Choco v 2.1 example 
% sample.scheduling.Rehearsal, the one defined in main() .
% (see rehearsal_choco.dta)
% num_pieces = 5;
% num_players = 3;
% duration = [4,6,3,5,7];
% rehearsal =  array2d(1..num_players, 1..num_pieces, 
%         [
%         1,1,0,1,0,
%         0,1,1,0,1,
%         1,1,0,1,1   
%   ]);


output[
  "order: " , show(rehearsal_order), "\n",
  "waiting_time: ", show(waiting_time), "\n",
  "total_waiting_time: " , show(total_waiting_time), "\n",
] ++ 
[
  if j = 1 then "\n" else " " endif ++
    show(rehearsal[p, rehearsal_order[j]]) ++ " "
  | p in 1..num_players, j in 1..num_pieces, 
] ++ 
["\n"]
;

