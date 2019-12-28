#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Magic square in Z3
#
# Magic square problem.
#
# For n=5:
# """
# Testing  5
# sat
# Time to sat:  9.268678
# s: 65
#  3  6 15 19 22
# 18 24  2  1 20
# 25 11 12  7 10
#  5 16 23 17  4
#  14  8 13 21  9
#  Time all:  18.497464
# """
# Why does it take 9 seconds to print?
  
#
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
# 
from z3_utils_hakank import *
import time

def print_square(mod,x,n):
    for i in range(n):
        for j in range(n):
            print("%2s" % mod.eval(x[(i, j)]),end=" ")
        print()
    

def magic_square(n,all=0):
    start = time.clock()
    sol = Solver()
    x = {}
    for i in range(n):
        for j in range(n):
            x[(i, j)] = Int("x(%i,%i)" % (i, j))
            # x[(i, j)] = BitVec("x(%i,%i)" % (i, j), 32) # Slower
            sol.add(x[(i,j)] >= 1,  x[(i,j)] <= n*n)
        
    s = Int("s")
    # s = BitVec("s",32)
    # s =   n*(nn+1)//2 # magical sum

    sol.add(s >= 1, s <= n*n*n)
    sol.add(Distinct([x[(i, j)] for i in range(n) for j in range(n)])) # flattened
    [sol.add(Sum([x[(i, j)] for j in range(n)]) == s) for i in range(n)]
    [sol.add(Sum([x[(i, j)] for i in range(n)]) == s) for j in range(n)]

    sol.add(Sum([x[(i, i)] for i in range(n)]) == s)  # diag 1
    sol.add(Sum([x[(i, n - i - 1)] for i in range(n)]) == s)  # diag 2

    # symmetry breaking
    sol.add(x[(0,0)] == 1)

    print(sol.check())
    end = time.clock()
    value = end - start
    print("Time to sat: ", value)
    if sol.check() == sat:
        mod = sol.model()
        print("s:", mod.eval(s))
        print_square(mod, x,n)
    else:
        print("No solution found!")

    end = time.clock()
    value = end - start
    print("Time all: ", value)


for n in [3,4,5]:
    print("Testing ", n)
    magic_square(n)
    print()
