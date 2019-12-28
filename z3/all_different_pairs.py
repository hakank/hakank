#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# All different pairs in Z3
#
# Assumption: a is a k by 2 matrix. n is the number of nodes.
#
# This model implements these decompositions:
#
#  - pairs(x,n): function which returns the pairs of matrix x
#    in "integer representation": a[k,1]*(n-1) + a[k,2]
#
#  - all_different_pairs(sol, x,n): all the pairs in x must be different
#
#  - increasing_pairs(sol, x,n): the pairs in x is in increasing order
#
#  - decreasing_pairs(sol, x,n): the pairs in x is in decreasing order
#
# n  #solutions
# -------------
# 1      0
# 2      1
# 3     12 
# 4    377
# 5  53834
# 6
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

# data
n = 5
m = n*(n - 1) // 2

print("n:",n, "m:", m)


# variables
x = {}
for i in range(m):
    for j in range(2):
        x[(i,j)] = makeIntVar(sol,"x[%i,%i]"%(i,j),1,n)

# constraints

all_different_pairs(sol, x, n)
increasing_pairs(sol,x, n)

for k in range(m):
    sol.add(x[(k,0)] != x[(k,1)])



num_solutions = 0
while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    for i in range(m):
        for j in range(2):
            print(mod[x[(i,j)]],end=" ")
        print()
    print()
    getDifferentSolutionMatrix(sol,mod,x,m,2)

print("num_solutions:", num_solutions)
