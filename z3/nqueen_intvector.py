#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# nqueen using IntVector  in Z3
#
# Faster than nqueen.py
# (though n=100 in 25.8s is not very impressive)
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

    q = makeIntVector(sol,"q",n,0,n-1)

    # Constraints
    sol.add(Distinct(q))
    sol.add(Distinct([q[i]+i for i in range(n)]))
    sol.add(Distinct([q[i]-i for i in range(n)]))

    if sol.check() == sat:
        mod = sol.model()
        ss = evalArray(mod, q)
        if all==0:
            print ss
        # Show all solutions
        if all==1:
            num_solutions = 0
            while sol.check() == sat:
                mod = sol.model()
                ss = [mod.evaluate(q[i]) for i in range(n)]
                # sol.add( Or([q[i] != ss[i] for i in range(n)]) )
                print "q=",ss
                num_solutions = num_solutions + 1
                getDifferentSolution(sol,mod,q)
            print "num_solutions:", num_solutions
    else:
        print "failed to solve"

    end = time.clock()
    value = end - start
    print "Time: ", value


for n in [8,10,12,20,25,30,100]:
    print "Testing ", n
    queens(n,0)
    print

# Show all 92 solutions
# queens(8,1)
