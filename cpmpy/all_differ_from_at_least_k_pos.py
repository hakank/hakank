"""
Global constraint all_differ_from_at_least_k_pos in cpmpy.

Global Constraint Catalogue
http://www.emn.fr/z-info/sdemasse/gccat/Call_differ_from_at_least_k_pos.html
'''
Enforce all pairs of distinct vectors of the VECTORS collection to differ 
from at least K positions.

Example
(
 2, <
 vec-<2, 5, 2, 0>,
 vec-<3, 6, 2, 1>,
 vec-<3, 6, 1, 0>
 >
)

The all_differ_from_at_least_k_pos constraint holds since:
 * The first and second vectors differ from 3 positions, which is 
   greater than or equal to K=2.
 * The first and third vectors differ from 3 positions, which is greater 
   than or equal to K=2.
 * The second and third vectors differ from 2 positions, which is greater 
   than or equal to K=2.
'''


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
from cpmpy import *
import numpy as np
from cpmpy_hakank import *


#
# all_differ_from_at_least_k_pos(k, x)
#
# Ensure that all pairs of vectors has >= k different values
#
def all_differ_from_at_least_k_pos(k, vectors):
    n = len(vectors)
    m = len(vectors[0])
    constraints = []
    for i in range(n):
        for j in range(i+1,n):
            constraints += [sum([vectors[i][kk] != vectors[j][kk] for kk in range(m)]) >= k]
    return constraints

#
# all_differ_from_exact_k_pos(k, vectors)
#
# Ensure that all pairs of vectors has exactly k different values
#
def all_differ_from_exact_k_pos(k, vectors):
    n = len(vectors)
    m = len(vectors[0])
    constraints = []
    for i in range(n):
        for j in range(i+1,n):
            constraints += [sum([vectors[i][kk] != vectors[j][kk] for kk in range(m)]) == k]
    return constraints

#
# all_differ_from_at_most_k_pos(k, x)
#
# Ensure that all pairs of vectors has <= k different values
#
def all_differ_from_at_most_k_pos(k, vectors):
    n = len(vectors)
    m = len(vectors[0])
    constraints = []
    for i in range(n):
        for j in range(i+1,n):
            constraints += [sum([vectors[i][kk] != vectors[j][kk] for kk in range(m)]) <= k]
    return constraints


def all_differ_from_at_most_k_pos_test():

    model = Model()

    # The example above, with a missing value
    # x_pre = [[2,5,2,-1],
    #          [3,6,2,1],
    #          [3,6,1,0]
    #     ]
    
    x_pre = [[3,6,3,-1],
             [3,6,2,1],
             [3,6,1,0]]
    
    rows = len(x_pre)
    cols = len(x_pre[0])
    k = 2

    # variables
    x = intvar(0,6,shape=(rows,cols),name="x")

    for i in range(rows):
        for j in range(cols):
            if x_pre[i][j] != -1:
                model += [x[i,j] == x_pre[i][j]]
                    

    # constraints
    model += [all_differ_from_at_least_k_pos(k, x)]
    # model += [all_differ_from_exact_k_pos(k, x)]
    # model += [all_differ_from_at_most_k_pos(k, x)]

    print("model:",model)

    def print_sol():
        print(x.value())
        print()

    model.solveAll(display=print_sol)


all_differ_from_at_most_k_pos_test()
