"""
Football optimization in cpmpy.

From the lp_solve mailing list:
http://tech.groups.yahoo.com/group/lp_solve/message/10450
'''
To: lp_solve@yahoogroups.com
From hakank  Mon May 26 00:45:06 2008
From: gandtandb <gandtandb@yahoo.co.uk>
Date: Sun, 25 May 2008 22:44:30 -0000
Subject: [lp_solve] Any Way To Make My LP_Solve Code Shorter, Please?
X-Mailer: Yahoo Groups Message Poster

To practice my new LP_Solve skills, I devised a question, and then
wrote some lp_solve code which answers the question correctly. My only
objection was that writing out the 33 lines of code was a little bit
tedious - was there a quicker way I could have done it, please?

Thanks for any advice!

Here's the question:

Congratulations! You've just managed your football team to promotion
into the Premiership - the top league in English football. The next
problem is to avoid immediate relegation back down - which often
happens to newly promoted teams.

Statistics show that, in the Premiership, there is a strong
correlation between the value of a team's players and their final
position in that league - so you want to spend as much money as you
can. The chairman gives you an upper limit of GBP 30 million - so you
want your purchases to add up to as close to that figure as possible
(without going over).

Here are the groups of players available, their price (in GBP
millions) and the numbers of each type you are obliged to buy:

Goalkeepers (must buy 1 exactly):
g1: 0.73
g2: 1.28
g3: 3.88

Defenders (must buy 2 or more):
d1: 0.92
d2: 1.31
d3: 1.62
d4: 2.41
d5: 2.79
d6: 3.28
d7: 3.91
d8: 4.57

Midfielders (must buy 3 or more):
m1: 1.8
m2: 2.63
m3: 3.17
m4: 3.769
m5: 4.14
m6: 4.75
m7: 5.38
m8: 5.93
m9: 6.78
m10: 7.13

Strikers (must buy 2 or more):
s1: 4.46
s2: 6.47
s3: 7.78
s4: 8.39
s5: 9.5
'''

There is 60 optimal solutions with z = 30000 (max budget).
Note that the number of chosen players are different: 8, 9, or 10.

Here is one solution:
  x:
  [[1 0 0 0 0 0 0 0 0 0]
   [0 1 0 1 0 0 0 0 0 0]
   [0 1 0 0 0 1 0 1 0 0]
   [1 0 1 0 0 0 0 0 0 0]]
  z: 30000
  num_player_chosen: 8


If we add the constraint that we want 11 players or more there is
one solution with a slightly less z:
  x:
  [[0 1 0 0 0 0 0 0 0 0]
   [1 1 1 1 1 0 0 0 0 0]
   [1 0 1 1 0 0 0 0 0 0]
   [1 1 0 0 0 0 0 0 0 0]]
  z: 29999
  num_player_chosen: 11


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys,math
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *
from itertools import combinations
import copy

def football(budget,num_types,num_players,max_num_players,min_max,costs):
    
    # the decision variables, i.e. whether we should buy a player or not.
    x = boolvar(shape=(num_types,max_num_players),name="x")
                
    # the total cost of the choosen players
    z = intvar(0,np.sum(costs),name="z")
    num_player_chosen = intvar(0,sum(num_players),name="num_player_chosen")
    

    model = Model([z == sum([x[i,j]*costs[i][j] for i in range(num_types) for j in range(max_num_players) if costs[i][j] > 0]),
                   num_player_chosen == x.sum()
        ])

    # minimum/maximum of players to buy
    for i in range(num_types):
        s = intvar(min_max[i][0],min_max[i][1],name=f"s[{i}]")
        model += [s == sum([x[i,j] == 1 for j in range(max_num_players) if costs[i][j] > 0])]

   
    # consider only the "real" players, i.e. not the "0" filled
    for i in range(num_types):
        for j in range(max_num_players):
            model += [(costs[i][j] == 0)<=(x[i,j] == 0)]
   
    model += [z <= budget]
    # model += [num_player_chosen >= 11] # I added this constraint for fun :-)

    model_opt = copy.copy(model)
    model_opt.maximize(z)
    model_opt.solve()
    opt_z = z.value()
    print("opt:", opt_z)
    model += (z == opt_z)

    def print_sol():
        print("x:")
        print(x.value())
        print("z:",z.value())
        print("num_player_chosen:",num_player_chosen.value())
        print()

    ss = CPM_ortools(model)
    num_solutions = ss.solveAll(display=print_sol)
    print("number of solutions:", num_solutions)
    print()

# Multiply money values with 1000
budget = 30000
num_players = [3, 8, 10, 5]
num_types = len(num_players)
max_num_players = max(num_players)
# min/max of players to buy
min_max = [[1, 1],
           [2, max_num_players],
           [3, max_num_players],
           [2, max_num_players]]

# cost matrix
costs = [[ 730, 1280, 3880,    0,    0,    0,    0,    0,    0,    0],
         [ 920, 1310, 1620, 2410, 2790, 3280, 3910, 4570,    0,    0],
         [1800, 2630, 3170, 3769, 4140, 4750, 5380, 5930, 6780, 7130],
         [4460, 6470, 7780, 8390, 9500,    0,    0,    0,    0,    0]]

football(budget,num_types,num_players,max_num_players,min_max,costs)
