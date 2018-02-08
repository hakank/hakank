#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# nqueen using Array in Z3
#
# Time for n=100
#  * nqueen.py: ??
#  * nqueen_intvector.py: 25.8s
#  * nqueen_array.py (this model): 15.1s (using IntSort(), BitVecSort(8))
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

    # q = Array("q", IntSort(), IntSort()) # much slower with IntSort()
    # q = Array("q", BitVecSort(8), BitVecSort(8)) # slower
    q = Array("q", IntSort(), BitVecSort(8))

    # Domains
    sol.add([And(q[i]>=0, q[i] <= n-1) for i in range(n)])
    # This is slower
    # for i in range(n):
    #   sol.add(And(q[i]>=0,q[i] <= n-1))

    # Constraints
    # sol.add(Distinct(q)) # Note: for arrays one must explicit enumerate all elements!
    sol.add(Distinct([q[i] for i in range(n)]))
    sol.add(Distinct([q[i]+i for i in range(n)]))
    sol.add(Distinct([q[i]-i for i in range(n)]))

    if sol.check() == sat:
        m = sol.model()
        # print "m:", m
        ss = [m.eval(q[i]) for i in range(n)]
        print ss
        # Show all solutions
        if all==1:
            count = 0
            while sol.check() == sat:
                m = sol.model()
                ss = [m.evaluate(q[i]) for i in range(n)]
                sol.add( Or([q[i] != ss[i] for i in range(n)]) )
                print "q=",ss
                count = count + 1

            print "count:", count
    else:
        print "failed to solve"

    end = time.clock()
    value = end - start
    print "Time: ", value


for n in [8,10,12,20,25,30,100]:
    print "Testing ", n
    queens(n,0)

# Show all 92 solutions
# queens(8,1)    
