#  
# Knapsack problem in z3.
#
# From
# http://rosettacode.org/wiki/Knapsack_problem/Unbounded
# """
# A traveller gets diverted and has to make an unscheduled stop in what turns out to 
# be Shangri La. Opting to leave, he is allowed to take as much as he likes of the 
# following items, so long as it will fit in his knapsack, and he can carry it. He knows 
# that he can carry no more than 25 'weights' in total; and that the capacity of his 
# knapsack is 0.25 'cubic lengths'.
#
# Looking just above the bar codes on the items he finds their weights and volumes. 
# He digs out his recent copy of a financial paper and gets the value of each item.
#
# Item	Explanation	                               Value (each) weight  Volume (each)
# ------------------------------------------------------------
# panacea (vials of)	Incredible healing properties	3000	0.3	0.025
# ichor (ampules of)	Vampires blood	                1800	0.2	0.015
# gold (bars)	        Shiney shiney	                2500	2.0	0.002
# ----------------------------------------------------------------------------
# Knapsack	        For the carrying of	-                <=25	<=0.25 
#
# He can only take whole units of any item,, but there is much more of any item than 
# he could ever carry
#
# How many of each item does he take to maximise the value of items he is carrying 
# away with him?
#
# Note:
#
#    1. There are four solutions that maximise the value taken. Only one need be given. 
# """

#
# Note that we must use ToReal on x[i], otherwise it's integer multiplication.
# 
# Here are the 4 solutions:
# 
# z: 54500
# x: [9, 0, 11]
# We take 9 of panacea.
# We take 11 of gold.
#
# total value: 54500.0
# total volume: 0.247
# total weight: 24.7
#
# 
# z: 54500
# x: [0, 15, 11]
# We take 15 of ichor.
# We take 11 of gold.
#
# total value: 54500.0
# total volume: 0.24699999999999997
# total weight: 25.0
#
# 
# z: 54500
# x: [3, 10, 11]
# We take 3 of panacea.
# We take 10 of ichor.
# We take 11 of gold.
#
# total value: 54500.0
# total volume: 0.247
# total weight: 24.9
#
#
# z: 54500
# x: [6, 5, 11]
# We take 6 of panacea.
# We take 5 of ichor.
# We take 11 of gold.
#
# total value: 54500.0
# total volume: 0.24700000000000003
# total weight: 24.8

#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 

from z3 import *

def knapsack(w,take,wtmax):
    # Note: ToReal(take[i])
    return Sum([w[i]*ToReal(take[i]) for i in range(len(w))]) <= wtmax

def knapsack_problem(opt_val=None):

    if opt_val == None:
        s = Optimize()
    else:
        s = SimpleSolver()

    items = ["panacea","ichor","gold"]
    value  = [3000.0, 1800.0, 2500.0  ]
    weight = [   0.3,    0.2,    2.0  ]
    volume = [   0.025,  0.015,  0.002]

    n = len(value)

    x = [Int(f"x[{i}]") for i in range(n)]
    for i in range(n):
        s.add(x[i] >= 0)

    z = Real("z")
    s.add(z >= 0)
    s.add(z == Sum([ToReal(x[i])*value[i] for i in range(n)]))

    s.add(knapsack(volume, x, 0.25))
    s.add(knapsack(weight, x, 25.0))

    if opt_val == None:
        s.maximize(z)
    else:
        s.add(z == opt_val)

    while s.check() == sat:
        mod = s.model()
        print("z:", mod[z].as_decimal(6))
        print("x:", [mod[x[i]] for i in range(n)])
        for i in range(n):
            if mod[x[i]].as_long() > 0:
                print(f"We take {mod[x[i]]} of {items[i]}.")
        print()
        print("total value:", sum([mod[x[i]].as_long()*value[i] for i in range(n)]))        
        print("total volume:", sum([mod[x[i]].as_long()*volume[i] for i in range(n)]))
        print("total weight:", sum([mod[x[i]].as_long()*weight[i] for i in range(n)]))        
        print()
        if opt_val == None:
            return mod[z].as_long()
        else:
            s.add(Or([x[i] != mod[x[i]] for i in range(n)]))


opt = knapsack_problem(None)
print("opt:", opt)
knapsack_problem(opt)

