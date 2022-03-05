#!/usr/bin/python -u
# -*- coding: latin-1 -*-
#
# Decomposition of diffn constraint.
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from __future__ import print_function
from z3_utils_hakank import *

def diffn_test():

    # sol = SolverFor("QF_LIA")
    sol = SimpleSolver()
    
    n = 14 # size of main square
    a = [1,1,1,1,2,3,3,3,5,6,6,8] # Sizes

    # n = 6
    # a = [1,1,1,1,1,2,3,3,3]

    size = len(a)

    x = makeIntVector(sol,"x",size,1,n)
    y = makeIntVector(sol,"y",size,1,n)

    # constraints

    diffn(sol,x,y,a,a),

    # cumulative(sol, start_times, duration, demand, num_resources,0,max_end_time)
    cumulative(sol,x,a,a,n,0,size)
    cumulative(sol,y,a,a,n,0,size)

    for i in range(size):
        sol.add(x[i] + a[i] <= n+1)
        sol.add(y[i] + a[i] <= n+1)

    # solution and search result
    num_solutions = 0
    while sol.check() == sat:
        num_solutions += 1
        mod = sol.model()
        print("x  :", [mod.eval(x[i]) for i in range(size)])
        print("y  :", [mod.eval(y[i]) for i in range(size)])
        print()
        getDifferentSolution(sol,mod,x,y)
        # There should be some difference in either x or y...
        # sol.add(Or([ Or([x[i] != mod[x[i]], y[i] != mod[y[i]]]) for i in range(size)] ))
    
    print("num_solutions:", num_solutions)


diffn_test()
