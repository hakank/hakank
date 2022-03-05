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

def queens(n,num_sols=0,print_sol=True):
    start = time.time()
    # sol = Solver()
    sol = SolverFor("NIA")

    q = makeIntVector(sol,"q",n,0,n-1)

    # Constraints
    sol.add(Distinct(q))
    sol.add(Distinct([q[i]+i for i in range(n)]))
    sol.add(Distinct([q[i]-i for i in range(n)]))

    num_solutions = 0
    while sol.check() == sat :
        mod = sol.model()
        ss = [mod.evaluate(q[i]) for i in range(n)]
        # sol.add( Or([q[i] != ss[i] for i in range(n)]) )
        if print_sol:
            print("q=",ss)
        num_solutions = num_solutions + 1
        if num_sols > 0 and num_solutions >= num_sols:
            break            
        getDifferentSolution(sol,mod,q)

    print("num_solutions:", num_solutions)

    end = time.time()
    value = end - start
    print("Time: ", value)

#
# Print first solution for some n.
#
def test1():

    for n in [8,10,12,20,25,30,50,100]:
        print("Testing ", n)
        queens(n,1,True)
        print()

# Show all 92 solutions
def test2():
    queens(8,0,True)



#
# Show all solutions from 8..12
#
# n: 8
# num_solutions: 92
# Time:  0.3076353073120117
#
# n: 9
# num_solutions: 352
# Time:  1.2901909351348877
#
# n: 10
# num_solutions: 724
# Time:  3.799804449081421
#
# n: 11
# num_solutions: 2680
# Time:  22.134512901306152
#
# n: 12
# num_solutions: 14200
# Time:  297.05348944664
#
def test3():
    for n in range(8,12+1):
        print("\nn:", n)
        queens(n,True,False)
    

test1()
# test2()
# test3()
