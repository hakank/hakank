#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Least diff problem in Z3
#
# The model solves the following problem:
# 
# What is the smallest difference between two numbers X - Y
# if you must use all the digits (0..9) exactly once, i.e.
# Minimize the difference 
#   ABCDE - FGHIJ
#
# Also, this model compares between using Optimize() and Solver()
# and we see that using different ordering of the constraints yield
# different intermediate solutions for this optimization problem.
# 
#
# This z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my z3 page: http://hakank.org/z3/
# 
# 
from z3_utils_hakank import *

#
# This version use Optimize() where we only see the final optimal solution.
# 
def least_diff_optimize():
    print("Using Optimize()")
    
    sol = Optimize()

    a,b,c,d,e = Ints("a b c d e")
    f,g,h,i,j = Ints("f g h i j")
    diff = Int("diff")

    t = [a,b,c,d,e, f,g,h,i,j]
    for x in t:
        sol.add(x >= 0)
        sol.add(x <= 9)

    sol.add(Distinct(t))
    sol.add(diff == (10000*a+1000*b+100*c+10*d+e)-(10000*f+1000*g+100*h+10*i+j))
    sol.add(diff >= 0)

    sol.minimize(diff)
    if sol.check() == sat:
        mod = sol.model()
        print("diff=",mod.evaluate(diff))
        print(evalArray(mod,t)) # [mod.evaluate(x) for x in t]
    else:
        print("No solution")

#
# This version use Solver and we manually ensure optimality
# and we see the how the value diff evolves
#
def least_diff_solver(type=1):
    print("Using Solver()")
    sol = Solver()

    a,b,c,d,e = Ints("a b c d e")
    f,g,h,i,j = Ints("f g h i j")
    diff = Int("diff")

    t = [a,b,c,d,e, f,g,h,i,j]
    for x in t:
        sol.add(x >= 0)
        sol.add(x <= 9)

    # The order of the constraints matter:
    # This give a faster solution
    if type == 1:
        sol.add(Distinct(t))
        sol.add(diff == (10000*a+1000*b+100*c+10*d+e)-(10000*f+1000*g+100*h+10*i+j))
        sol.add(diff >= 0)
    else:
        # .. than this, where Distinct is last
        sol.add(diff == (10000*a+1000*b+100*c+10*d+e)-(10000*f+1000*g+100*h+10*i+j))
        sol.add(diff >= 0)
        sol.add(Distinct(t))

    while sol.check() == sat:
        mod = sol.model()
        print("diff=",mod.evaluate(diff))
        print(evalArray(mod,t)) # [mod.evaluate(x) for x in t]
        getLessSolution(sol,mod, diff)

least_diff_optimize()
print()
least_diff_solver(1)
print()
least_diff_solver(2)
