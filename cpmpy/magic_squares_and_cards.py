"""
Magic squares and cards problem in cpmpy.

Martin Gardner (July 1971)
'''
Allowing duplicates values, what is the largest constant sum for
an order-3 magic square that can be formed with nine cards from the deck.
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

# This is little too messy...
def print_solution(a):
    x = a[0]
    print("x:")
    for i in range(3):
        for j in range(3):
            print(f"{x[i*3+j].value():3d}",end=" ")
        print()
    print('s:', a[1][0].value())
    print("counts:", a[2].value())


def magic_squares_and_cards():

    n = 3
    x = intvar(1,13,shape=(n, n), name='x')
    x_flat = [x[i][j] for i in range(n) for j in range(n)]
    # The total
    s = intvar(0, 13*4, name='s') 

    # count of each "card" (atmost 4)
    counts = intvar(0, 4, shape=13+1, name='counts')
    
    model = Model (
        [
        # there are 4 cards of each value in a deck       
        global_cardinality_count(x_flat,counts),
        
        # the standard magic square constraints (sans all_different)
        [ sum(row) == s for row in x],
        [ sum(col) == s for col in x.transpose()],               
        sum([ x[i,i] for i in range(n)]) == s, # diag 1
        sum([ x[i,n-i-1] for i in range(n)]) == s, # diag 2

        ],
        maximize=s,
        )

    ortools_wrapper_opt(model,[x_flat,[s],counts],print_solution)
    
magic_squares_and_cards()


