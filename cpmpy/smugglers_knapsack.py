"""
Smuggler's knapsack problem in cpmpy.

Marriott & Stucker: 'Programming with constraints', page  101f, 115f

Smuggler's knapsack.
  
A smuggler has a knapsack with a capacity of 9 units.
            Unit       Profit
Whisky:     4 units    15 dollars
Perfume:    3 units    10 dollars
Cigarettes: 2 units     7 dollars

What is the optimal choice?


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys, math,string
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

#
# "Direct" approach
#
def smugglers_knapsack1():

    max_val = 9

    x = intvar(0,9,shape=3,name="x")
    whisky,perfume,cigarettes = x
    profit = intvar(0,1000,name="profit")
    
    #       Unit       Profit
    # Whisky:     4 units    15 dollars
    # Perfume:    3 units    10 dollars
    # Cigarettes: 2 units     7 dollars

    
    model = Model([4*whisky  + 3*perfume  + 2*cigarettes <= max_val, # Units
                   15*whisky + 10*perfume + 7*cigarettes == profit
                  ])

    model.maximize(profit)

    print(model)

    def print_sol():
        print("whisky:",whisky.value())
        print("perfume:",perfume.value())
        print("cigarettes:",cigarettes.value())
        print("profit:",profit.value())
        print()
        
    ss = CPM_ortools(model)    
    # ss.ort_solver.parameters.max_time_in_seconds = timeout
    # ss.ort_solver.parameters.num_search_workers = num_procs 
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    # ss.ort_solver.parameters.linearization_level = 0
    # ss.ort_solver.parameters.cp_model_probing_level = 0

    num_solutions = ss.solveAll(display=print_sol)
    print("num_solutions:",num_solutions)
    print()

#
# General approach.
# 
def smugglers_knapsack2(units,values,max_val):

    n = len(units)

    # variables
    x = intvar(0,100,shape=n,name="x")
    profit = intvar(0,1000,name="profit")

    model = Model([sum(x*units) <= max_val,
                   profit == sum(x*values),
                   ],
                  maximize=profit
                  )

    print(model)

    def print_sol():
        print("x:",x.value())
        print("profit:",profit.value())
        print()
        
    ss = CPM_ortools(model)    
    num_solutions = ss.solveAll(display=print_sol)
    print("num_solutions:",num_solutions)
    print()
    
print("smugglers_knapsack1:")
smugglers_knapsack1()
print()

print("\nsmugglers_knapsack2:")
units  = [4,3,2]
values = [15,10,7]
max_val = 9
smugglers_knapsack2(units,values,max_val)
