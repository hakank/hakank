"""
12-pack problem in cpmpy.

From Mathias Brandewinder:
'The 12 pack-problem: combination of integers'
http://clear-lines.com/blog/post/The-12-pack-problem-combination-of-integers.aspx
'''
A fun problem came my way today. Imagine that you are the owner 
of a renowned brewery in Bizzarostan, a country where beer is sold 
only in 7-packs and 13-packs, sometimes described as the ++ packs. 
Beer is a serious matter in Bizzarostan, and buying single bottles is 
not tolerated by the law.

You take great pride in doing what's best for your customers, 
so when a customers asks you for, say, 20 beers, you always try your 
best to find the combination of 7-packs and 13-packs that will meet 
your customer’s thirst, for the least amount of hard-earned money - 
in that case, a 7-pack and a 13-pack.

To be extra clear, the goal is to find a combination of 7- and 
13-packs containing at least as many bottles as requested, 
with a total number of bottles as close as possible to the amount requested.
'''

Also see the follow up post:
'12-pack,take one: a Sieve like approach'
http://clear-lines.com/blog/post/12-pack-take-one-a-Sieve-like-approach.aspx
'''
Suppose that you are given a list of integers, and a target integer. 
Your goal is to find the closest value that is greater or equal 
to the target, by combining integers ('packs') from the list (only 
positive combinations are allowed). For instance, given 3 and 5, 
the closest you can get to 16 would be 5 x 2 + 3 x 2 = 16, and the 
closest to 17 would be 3 x 6 = 18.
'''

Related:
- https://mathworld.wolfram.com/CoinProblem.html
- https://mathworld.wolfram.com/FrobeniusNumber.html
- http://hakank.org/picat/mcnuggets_problem.pi
- http://hakank.org/picat/frobenius_number.pi



Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys,random
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


  
def n_pack_problem(target,given,max_val):
    print("target:",target, "given:",given)
    
    n = len(given)

    # variables
    x = intvar(0,max_val,shape=n,name="x")
    total = intvar(0,max_val*n,name="total")

    # constraints
    model = Model([total == (x*given).sum(),
                   total >= target,
                   ])

    model.minimize(total)

    ss = CPM_ortools(model)
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH 
    # ss.ort_solver.parameters.cp_model_presolve = False
    ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0

    num_solutions = 0
    opt = None
    while(ss.solve()):
        num_solutions += 1
        total_val = total.value()
        diff = total_val - target
        print("x:",x.value(), "total:",total_val, "exact!" if diff == 0 else f"diff {diff}", flush=True)
        if opt == None:
            ss += [total == total_val]
        ss += [x != x.value()]

    # print()
    # print("num_solutions:", num_solutions)
    # print("Num conflicts:", ss.ort_solver.NumConflicts())
    # print("NumBranches:", ss.ort_solver.NumBranches())
    # print("WallTime:", ss.ort_solver.WallTime())
    print()

# target = 1230
# given = [8,13,21,34] # list of given numbers

given = sorted(set([random.randint(3,100) for i in range(random.randint(3,10))]))

# given = [7,13]
max_val = 1000 # Arbitrary max limit of amount
for target in range(1,100):
    n_pack_problem(target,given,max_val)
