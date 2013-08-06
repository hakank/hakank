% 
% Football optimization in MiniZinc.
%
% Generalized solution of football.mzn. 
% Also changed to integer version, since more solvers can handle IP problems.
% 
% From the lp_solve mailing list:
% http://tech.groups.yahoo.com/group/lp_solve/message/10450
% """
% To: lp_solve@yahoogroups.com
% From hakank  Mon May 26 00:45:06 2008
% From: gandtandb <gandtandb@yahoo.co.uk>
% Date: Sun, 25 May 2008 22:44:30 -0000
% Subject: [lp_solve] Any Way To Make My LP_Solve Code Shorter, Please?
% X-Mailer: Yahoo Groups Message Poster

% To practice my new LP_Solve skills, I devised a question, and then
% wrote some lp_solve code which answers the question correctly. My only
% objection was that writing out the 33 lines of code was a little bit
% tedious - was there a quicker way I could have done it, please?

% Thanks for any advice!

% Here's the question:

% Congratulations! You've just managed your football team to promotion
% into the Premiership - the top league in English football. The next
% problem is to avoid immediate relegation back down - which often
% happens to newly promoted teams.

% Statistics show that, in the Premiership, there is a strong
% correlation between the value of a team's players and their final
% position in that league - so you want to spend as much money as you
% can. The chairman gives you an upper limit of GBP 30 million - so you
% want your purchases to add up to as close to that figure as possible
% (without going over).

% Here are the groups of players available, their price (in GBP
% millions) and the numbers of each type you are obliged to buy:

% Goalkeepers (must buy 1 exactly):
% g1: 0.73
% g2: 1.28
% g3: 3.88

% Defenders (must buy 2 or more):
% d1: 0.92
% d2: 1.31
% d3: 1.62
% d4: 2.41
% d5: 2.79
% d6: 3.28
% d7: 3.91
% d8: 4.57

% Midfielders (must buy 3 or more):
% m1: 1.8
% m2: 2.63
% m3: 3.17
% m4: 3.769
% m5: 4.14
% m6: 4.75
% m7: 5.38
% m8: 5.93
% m9: 6.78
% m10: 7.13

% Strikers (must buy 2 or more):
% s1: 4.46
% s2: 6.47
% s3: 7.78
% s4: 8.39
% s5: 9.5
% """
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc


% Note: I changed to integer version since most solvers has problem with
% the float version.
% There are 60 solutions to the integer version. 
%
% Minizinc solution:
% z: 30000
% 0 0 1 0 0 0 0 0 0 0
% 1 0 0 0 0 0 1 0 0 0
% 1 1 0 0 0 0 0 1 0 0
% 1 1 0 0 0 0 0 0 0 0


int: budget;

% number of different types of players
int: num_types; 

% number of players for the specific type
array[1..num_types] of int: num_players;

% max number of players (the other rows are 0 filled)
int: max_num_players = max(num_players); 

% cost for the players
% since there are unequal number of different players, we fill with 0
array[1..num_types, 1..max_num_players] of int: cost;

% the decision variables, i.e. whether we should buy a player or not.
array[1..num_types, 1..max_num_players] of var 0..1: x;

% required minumum/maximum number of players to buy
array[1..num_types, 1..2] of int: min_max;

% the total cost of the choosen players
var int: z = sum(i in 1..num_types, j in 1..max_num_players where cost[i,j] > 0) (
                 x[i,j]*cost[i,j] 
             ); 

var int: num_players_choosen =  sum(i in 1..num_types, j in 1..max_num_players where cost[i,j] > 0) (
                 x[i,j] 
             ); 

solve :: int_search([x[i,j] | i in 1..num_types, j in 1..max_num_players], first_fail, indomain_min, complete) maximize z;
% solve satisfy;

constraint
   % minimum/maximum of players to buy
   forall(i in 1..num_types) (
      let {
        var int: s =  sum(j in 1..max_num_players where cost[i,j] > 0) (bool2int(x[i,j]=1))
      }
      in
       s >= min_max[i,1] 
       /\ 
       s <= min_max[i,2]      
   )
   /\ % consider only the "real" players, i.e. not the "0" filled
   forall(i in 1..num_types, j in 1..max_num_players) (
      cost[i,j] = 0 -> x[i,j] = 0
   )
   /\
   z <= budget
   % /\ num_players_choosen >= 11 % added this constraint for fun :-)
   % /\ z >= budget % for satisfy
   
;

%
% data
%

budget = 30000;

num_types = 4;
num_players = [3, 8, 10, 5];

% min/max of players to buy
% old minizinc
min_max = [|
   1,1,
   |2,max_num_players,
   |3,max_num_players,
   |2,max_num_players
|];

% new minizinc
% min_max = [|1,1,
%   |2,max_num_players,
%   |3,max_num_players,
%   |2,max_num_players|];


% cost matrix
% old minizinc, integer version
% Fills with 0 for N/A values
cost = 
   [| 730, 1280, 3880,    0,    0,    0,    0,    0,    0,    0,
    | 920, 1310, 1620, 2410, 2790, 3280, 3910, 4570,    0,    0,
    |1800, 2630, 3170, 3769, 4140, 4750, 5380, 5930, 6780, 7130,
    |4460, 6470, 7780, 8390, 9500,    0,    0,    0,    0,    0
|];


output 
[
  "z: ", show(z), "\n",
  "num_players_choosen: ", show(num_players_choosen),"\n"
]++
[
   if j = 1 then "\n" else " " endif ++
   show(x[i,j]) 
   | i in 1..num_types, j in 1..max_num_players
] ++ ["\n"];
