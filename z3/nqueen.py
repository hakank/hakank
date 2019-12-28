#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# n-queens problem in Z3
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
# 
from z3_utils_hakank import *
import time, datetime

def queens(n,all=0):
    start = time.clock()
    sol = Solver()
    # sol = SimpleSolver()
    # sol = SolverFor("QF_LIA")

    q = [Int("q_%s" % (i)) for i in range(n) ]

    # Domains
    sol.add([And(q[i]>=0, q[i] <= n-1) for i in range(n)])

    # Constraints
    sol.add(Distinct(q))
    sol.add(Distinct([q[i]+i for i in range(n)]))
    sol.add(Distinct([q[i]-i for i in range(n)]))

    if sol.check() == sat:
        mod = sol.model()
        ss = evalArray(mod, q)
        if all == 0:
            print(ss)
        # Show all solutions
        if all==1:
            num_solutions = 0
            while sol.check() == sat:
                mod = sol.model()
                ss = [mod.evaluate(q[i]) for i in range(n)]
                print("q=",ss)
                num_solutions += 1
                getDifferentSolution(sol,mod, q)
                # sol.add( Or([q[i] != ss[i] for i in range(n)]) )

            print("num_solutions:", num_solutions)
    else:
        print("failed to solve")

    end = time.clock()
    value = end - start
    print("Time: ", value)
    print()


for n in [8,10,12,20,50,100,200]:
    print("Testing ", n)
    queens(n,0)
    print()

# Show all solutions
# queens(8,1)
queens(7,1)

