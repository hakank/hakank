#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Global constraint all_different_on_intersection in Z3
#
# From Global Constraint Catalogue
# http://www.emn.fr/x-info/sdemasse/gccat/Calldifferent_on_intersection.html
# """
# The values that both occur in the VARIABLES1 and VARIABLES2 collections 
# have only one occurrence.
#
# Example
# (
#  <5, 9, 1, 5>,
#  <2, 1, 6, 9, 6, 2>
# )
#
# The alldifferent_on_intersection constraint holds since the values 9 and 1 
# that both occur in <5, 9, 1, 5> as well as in <2, 1, 6, 9, 6, 2> have 
# exactly one occurrence in each collection.
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *


sol = Solver()

m = 4
n = 6

x_init = [5,9,-1,5]
y_init = [2,1,6,9,6,2]

# variables
x = makeIntVector(sol, "x", m,1,9)
y = makeIntVector(sol, "y", n,1,9)

for i in range(m):
    if x_init[i] >= 0:
        sol.add(x[i] == x_init[i])

for i in range(n):
    sol.add(y[i] == y_init[i])

# constraints

all_different_on_intersection(sol,x,y)


num_solutions = 0
while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print("x:", [mod[x[i]] for i in range(m)])
    print("y:", [mod[y[i]] for i in range(n)])
    print()
    getDifferentSolution(sol,mod,x+y)

print("num_solutions:", num_solutions)
