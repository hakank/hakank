#  
#  Football optimization in z3.
#  
#  From the lp_solve mailing list:
#  http://tech.groups.yahoo.com/group/lp_solve/message/10450
#  """
#  To practice my new LP_Solve skills, I devised a question, and then
#  wrote some lp_solve code which answers the question correctly. My only
#  objection was that writing out the 33 lines of code was a little bit
#  tedious - was there a quicker way I could have done it, please?
#
#  Thanks for any advice!
#
#  Here's the question:
#
#  Congratulations! You've just managed your football team to promotion
#  into the Premiership - the top league in English football. The next
#  problem is to avoid immediate relegation back down - which often
#  happens to newly promoted teams.
#
#  Statistics show that, in the Premiership, there is a strong
#  correlation between the value of a team's players and their final
#  position in that league - so you want to spend as much money as you
#  can. The chairman gives you an upper limit of GBP 30 million - so you
#  want your purchases to add up to as close to that figure as possible
#  (without going over).
#
#  Here are the groups of players available, their price (in GBP
#  millions) and the numbers of each type you are obliged to buy:
#
#  Goalkeepers (must buy 1 exactly):
#  g1: 0.73
#  g2: 1.28
#  g3: 3.88
#
#  Defenders (must buy 2 or more):
#  d1: 0.92
#  d2: 1.31
#  d3: 1.62
#  d4: 2.41
#  d5: 2.79
#  d6: 3.28
#  d7: 3.91
#  d8: 4.57
#
#  Midfielders (must buy 3 or more):
#  m1: 1.8
#  m2: 2.63
#  m3: 3.17
#  m4: 3.769
#  m5: 4.14
#  m6: 4.75
#  m7: 5.38
#  m8: 5.93
#  m9: 6.78
#  m10: 7.13
#
#  Strikers (must buy 2 or more):
#  s1: 4.46
#  s2: 6.47
#  s3: 7.78
#  s4: 8.39
#  s5: 9.5
#  """
#
# hakank:
# Here we compare when the costs are floats (in GPB millions) or
# as integers (float values multiplied by 1000).
#
# I also added (fake) importance weights for the different types.
# The objective is then to maximize the total sum of weights (z).
#
# Here is the result when maximizing z
# Integer costs:
# z: 32
# tot_cost: 29289 num_players_choosen: 11
# Goalkeepers : x - - 
# Defenders   : x x x x - - - - 
# Midfielders : x x x x - - - - - - 
# Strikers    : x x - - - 

#
# Float costs:
# tot_cost: 29 num_players_choosen: 13
# Goalkeepers : x - - 
# Defenders   : x x x x x - - - 
# Midfielders : x x x x x - - - - - 
# Strikers    : x x - - -
# 
#
# Here is a variant where we maximize z and minimize tot_cost
#
# Integer cost:
# z: 31
# tot_cost: 28800 num_players_choosen: 11
# Goalkeepers : x - - 
# Defenders   : x x x x - x - - 
# Midfielders : x x x - - - - - - - 
# Strikers    : x x - - - 
#
# Float cost
# z: 34
# tot_cost: 25 num_players_choosen: 12
# Goalkeepers : x - - 
# Defenders   : x x x x x - - - 
# Midfielders : x x x x - - - - - - 
# Strikers    : x x - - - 
#


# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 

from z3 import *

def football(budget,types,min_max,cost,weights):
    
    # s = SimpleSolver()
    s = SolverFor("QF_LIRA")
    # s = Optimize() # too slow
    
    cost_type = "int"
    if isinstance(cost[0][0],float):
        cost_type = "float"

    cost_len = [len(cost[i]) for i in range(len(cost))]    
    num_types = len(types)
    
    # the decision variables, i.e. whether we should buy a player or not.
    x = [[Int(f"x[{i},{j}") for j in range(cost_len[i])] for i in range(num_types)]
    
    for i in range(num_types):
        for j in range(cost_len[i]):
            s.add(x[i][j] >= 0, x[i][j] <= 1)
            if cost[i][j] == 0:
                s.add(x[i][j] == 0)

    # the total cost of the choosen players
    if cost_type == "int":
        tot_cost = Int("tot_cost")
        z = Int("z")
    else:
        tot_cost = Real("tot_cost")
        z = Real("z")
        
    s.add(tot_cost == Sum([x[i][j]*cost[i][j] for i in range(num_types)
                           for j in range(cost_len[i])]))

    s.add(z == Sum([Sum([x[i][j] for j in range(cost_len[i])]) * weights[i] 
                    for i in range(num_types)]))


    num_players_choosen = Int("num_players_choosen")
    s.add(num_players_choosen == Sum([x[i][j] for i in range(num_types) for j in range(cost_len[i])]))


    # minimum/maximum of players to buy
    for i in range(num_types):
        s.add(Sum([If(x[i][j] == 1,1,0) for j in range(cost_len[i])]) >= min_max[i][0])
        s.add(Sum([If(x[i][j] == 1,1,0) for j in range(cost_len[i])]) <= min_max[i][1])
     
    s.add(tot_cost <= budget)
  
    # s.maximize(z)

    while s.check() == sat:
        mod = s.model()
        print("z:", mod[z])
        print("tot_cost:", mod[tot_cost],"num_players_choosen:", mod[num_players_choosen])
        for i in range(num_types):
            print(f"{types[i]:12}: ", end = "")
            for j in range(cost_len[i]):
                if mod[x[i][j]].as_long() == 1:
                    print("x", end = " ")
                else:
                    print("-", end = " ")
            print()
        print()        
        # s.add(num_players_choosen > mod[num_players_choosen])
        s.add(tot_cost > mod[tot_cost])
        # s.add(z > mod[z], tot_cost < mod[tot_cost])


types = ["Goalkeepers", "Defenders", "Midfielders","Strikers"]
# min/max of players to buy
min_max = [[1, 1],
           [2, 10],
           [3, 10],
           [2, 10]]

# cost matrix
budget_float = 30
cost_float = [[0.73, 1.28, 3.88],
              [0.92, 1.31, 1.62, 2.41, 2.79, 3.28, 3.91, 4.57],
              [1.8, 2.63, 3.17, 3.769, 4.14, 4.75, 5.38, 5.93, 6.78,  7.13],
              [4.46, 6.47,7.78,8.39,9.5]]

# Integers: float * 1000
budget_int = 30000
cost_int = [[ 730, 1280, 3880],
            [ 920, 1310, 1620, 2410, 2790, 3280, 3910, 4570],
            [1800, 2630, 3170, 3769, 4140, 4750, 5380, 5930, 6780, 7130],
            [4460, 6470, 7780, 8390, 9500]]

# hakank: Added some (fake) importance weights for each type.
weights = [4,2,3,4]

print("Integer cost:")
football(budget_int,types,min_max,cost_int,weights)

print("\nFloat cost:")
football(budget_float,types,min_max,cost_float,weights)


