#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# n-queens in Z3
#
# Alternative formulation compared to nqueen.py
# Slower.
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
# 
from z3 import *
import time

def queens(n,all=0):
    start = time.time()
    sol = SolverFor("QF_FD")

    q = IntVector("q", n) # this is much faster # n=100: 17.1s

    # Domains
    sol.add([And(q[i]>=0, q[i] <= n-1) for i in range(n)])

    # Constraints
    for i in range(n):
        for j in range(i):
            sol.add(q[i] != q[j], q[i]+i != q[j]+j, q[i]-i != q[j]-j)

    if sol.check() == sat:
        mod = sol.model()
        ss = [mod.evaluate(q[i]) for i in range(n)]
        print(ss)
        # Show all solutions
        if all==1:
            num_solutions = 0
            while sol.check() == sat:
                m = sol.model()
                ss = [mod.evaluate(q[i]) for i in range(n)]
                sol.add( Or([q[i] != ss[i] for i in range(n)]) )
                print("q=",ss)
                num_solutions = num_solutions + 1

            print("num_solutions:", num_solutions)
    else:
        print("failed to solve")

    end = time.time()
    value = end - start
    print("Time: ", value)


for n in [8,10,12,20,50,100,200]:
    print("Testing ", n)
    queens(n,0)

# Show all 92 solutions
# queens(8,1)    

