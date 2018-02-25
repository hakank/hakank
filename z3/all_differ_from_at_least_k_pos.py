#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Global constraint all_differ_from_at_least_k_pos in Z3
#
# Global Constraint Catalogue
# http://www.emn.fr/z-info/sdemasse/gccat/Call_differ_from_at_least_k_pos.html
# """
# Enforce all pairs of distinct vectors of the VECTORS collection to differ 
# from at least K positions.
# 
# Example
# (
#  2, <
#  vec-<2, 5, 2, 0>,
#  vec-<3, 6, 2, 1>,
#  vec-<3, 6, 1, 0>
#  >
#)
# 
# The all_differ_from_at_least_k_pos constraint holds since:
#  * The first and second vectors differ from 3 positions, which is 
#    greater than or equal to K=2.
#  * The first and third vectors differ from 3 positions, which is greater 
#    than or equal to K=2.
#  * The second and third vectors differ from 2 positions, which is greater 
#    than or equal to K=2.
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

#
# all_differ_from_at_least_k_pos(sol, k, x)
#
# Ensure that all pairs of vectors has >= k different values
#
def all_differ_from_at_least_k_pos(sol, k, vectors):
    n = len(vectors)
    m = len(vectors[0])
    for i in range(n):
        for j in range(i+1,n):
            sol.add(Sum([If(vectors[i][kk] != vectors[j][kk],1,0) for kk in range(m)]) >= k)

#
# all_differ_from_exact_k_pos(sol, k, vectors)
#
# Ensure that all pairs of vectors has exactly k different values
#
def all_differ_from_exact_k_pos(sol, k, vectors):
    n = len(vectors)
    m = len(vectors[0])
    for i in range(n):
        for j in range(i+1,n):
            sol.add(Sum([If(vectors[i][kk] != vectors[j][kk],1,0) for kk in range(m)]) == k)

#
# all_differ_from_at_most_k_pos(sol, k, x)
#
# Ensure that all pairs of vectors has <= k different values
#
def all_differ_from_at_most_k_pos(sol, k, vectors):
    n = len(vectors)
    m = len(vectors[0])
    for i in range(n):
        for j in range(i+1,n):
            sol.add(Sum([If(vectors[i][kk] != vectors[j][kk],1,0) for kk in range(m)]) <= k)


sol = Solver()



# x = [[2,5,2,0],
#      [3,6,2,1],
#      [3,6,1,0]
#     ]
# rows = len(x)
# cols = len(x[0])

rows = 3
cols = 4
k = 2

# variables
x = []
for i in range(rows):
    x.append([makeIntVar(sol,"x_%i_%i"%(i,j),1,3) for j in range(cols)])
        
print "x:", x

# constraints

all_differ_from_at_least_k_pos(sol, k, x)
# all_differ_from_exact_k_pos(sol, k, x)
# all_differ_from_at_most_k_pos(sol, k, x)

num_solutions = 0
while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    for i in range(rows):
        for j in range(cols):
            print mod[x[i][j]],
        print
    print
    sol.add(Or([x[i][j] != mod[x[i][j]] for i in range(rows) for j in range(cols)]))

print "num_solutions:", num_solutions

