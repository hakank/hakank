"""
3 coins problem in cpmpy.

From 
http://www.cs.cf.ac.uk/htbin/Dave/AI/ai.pl?AI1/means_end.html+agenda+game+AI1/AI1.html+Lecture_12:_Agendas+Lecture_14:_Game_Playing+AI_TOP_LEVEL+LECTURE_13:_Means_End_Analysis
'''
Three coins lie on a table in the order tails, heads ,tails. In precisely three moves 
make them face either all heads or all tails, GPS generated 10 goals.
'''

The problem has 7 solutions:

(all tails)
[[1 0 1]
 [0 0 1]
 [1 0 1]
 [1 1 1]]

(all tails)
[[1 0 1]
 [1 1 1]
 [1 0 1]
 [1 1 1]]

(all tails)
[[1 0 1]
 [1 0 0]
 [1 0 1]
 [1 1 1]]

(all tails)
[[1 0 1]
 [0 0 1]
 [0 1 1]
 [1 1 1]]

(all tails)
[[1 0 1]
 [1 1 1]
 [0 1 1]
 [1 1 1]]

(all tails)
[[1 0 1]
 [1 1 1]
 [1 1 0]
 [1 1 1]]

(all tails)
[[1 0 1]
 [1 0 0]
 [1 1 0]
 [1 1 1]]


With different init configurations there are slighly different number of solutions:
{(0, 0, 0): 6, (0, 0, 1): 7, (0, 1, 1): 7, (1, 1, 1): 6}

The configurations with 6 solutions ((0, 0, 0) and (1, 1, 1)) has all all heads
solutions, while those with 7 solutions ((0, 0, 1) and (0, 1, 1)) are all all tails.


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys,math
import numpy as np
import itertools
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def three_coins(init=[1,0,1]):
    print("init:",init)
    
    n = 3
    num_moves = 3

    # decision variables
    # 0: heads, 1: tails
    x = boolvar(shape=(num_moves+1,n),name="x")

    #  last line, either all heads or all tails 
    last_val = intvar(0,n,name="last_val")

    model = Model([ x[0] == init,
                    # Exactly one difference per move
                    [ sum([x[m,j] != x[m-1,j] for j in range(n)]) == 1 for m in range(1,num_moves+1)],

                    last_val == sum(x[num_moves]),
                    # last line: either all heads of all tails
                    ((last_val == 0) | (last_val == n)),
                    ])

    def print_sol():
        print(f"({'all tails' if last_val.value() == 3 else 'all heads'})")
        print(x.value())
        print()
    
    ss = CPM_ortools(model)
    num_solutions = ss.solveAll(display=print_sol)
    print("num_solutions:", num_solutions)
    print()
    return num_solutions

print("Original problem:")
init = [1,0,1] # original problem
three_coins()

# Testing all variants:
# [(0, 0, 0), (0, 0, 1), (0, 1, 1), (1, 1, 1)]
# The original problem (1,0,1) is by symmetry the same as (0,1,1)
#
print("\nTesting different configurations:\n")
num_sols = {} # number of solutions for the different init configurations
for init in list(itertools.combinations_with_replacement([0,1],3)):
    num_sols[init] = three_coins(init)
    print()


print("num_sols:")
print(num_sols)
