#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# 3SUM (Three Elements That Sum To Zero) in Z3
#
# From
# http://nathanleclaire.com/blog/2013/10/22/three-elements-that-sum-to-zero/
# """
# Given a collection of integers, return the indices of any three elements which sum to zero. 
# For instance, if you are given {-1, 6, 8, 9, 10, -100, 78, 0, 1}, you could return {0, 7, 8} 
# because -1 + 1 + 0 == 0. You can't use the same index twice, and if there is no match you 
# should return {-1, -1, -1}.
# """
#
# Also see: https://en.wikipedia.org/wiki/3SUM
#
# Note: This model skips the last requirement, i.e. the (-1,-1,-1)) if unsat.
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

n = 9
nums = [-1, 6, 8, 9, 10, -100, 78, 0, 1]
# nums = [1, 6, 8, 9, 10, 100, 78, 0, 1] # UNSAT

m = 3 # The number of elements that should add to 0

# variables

x = makeIntVector(sol,"x",n,0,1)

# constraints

sol.add(Sum([nums[i]*x[i] for i in range(n)]) == 0)
sol.add(Sum(x) == m)

num_solutions = 0
while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print "x: ", [mod[x[i]] for i in range(n)]
    print "x: ", [i for i in range(n) if mod[x[i]] == 1]
    print
    getDifferentSolution(sol,mod,x)
    

print "num_solutions:", num_solutions

