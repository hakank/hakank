#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Bowls and Oranges problem in Z3
#
# From BitTorrent Developer Challenge
# http://www.bittorrent.com/company/about/developer_challenge
# """
# You have 40 bowls, all placed in a line at exact intervals of 
# 1 meter. You also have 9 oranges. You wish to place all the oranges 
# in the bowls, no more than one orange in each bowl, so that there are 
# no three oranges A, B, and C such that the distance between A and B is 
# equal to the distance between B and C. How many ways can you arrange 
# the oranges in the bowls?.
# """
# 
# Via http://surana.wordpress.com/2011/06/01/constraint-programming-example/
#
# There are 7555794 ways.
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

# sol = Solver()
sol = SolverFor("QF_LIA")

## This takes _very_ long time (to be honest I haven't finished it...)
# n = 40 # number of bowls
# m = 9  # number of oranges

# There are 3452 different solutions to this configuration
n = 20 # number of bowls
m = 4  # number of oranges

x = makeIntVector(sol,"x",m,1,n)

sol.add(Distinct(x))
increasing(sol,x) 

for i in range(m):
    for j in range(m):
        for k in range(m):
            if i < j and j < k:
                sol.add(x[j]-x[i] != x[k]-x[j])


num_solutions = 0
while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    # print([mod[x[i]] for i in range(m)])
    if num_solutions % 1000 == 0:
        print(num_solutions)
    getDifferentSolution(sol,mod, x)

print("num_solutions:", num_solutions)
