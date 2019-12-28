#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Magic square in Z3
#
# Variant using an IntVector of size n*n instead of a could of Ints
# (compare with magic_square.py).
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
    nn = n*n
    # Here we define and IntVector instead
    # But almost everything else is the same as magic_square.py
    xi = IntVector("xxxz3", nn)
    
    x = {}
    for i in range(n):
        for j in range(n):
            x[(i, j)] = xi[(i-1)*n+j] # connect xi and x
            sol.add(x[(i,j)] >= 1,  x[(i,j)] <= n*n)
        
    s = Int("s")
    sol.add(s >= 1, s <= n*n*n)
    sol.add(Distinct([xi[i] for i in range(nn)])) # flattened
    [sol.add(Sum([x[(i, j)] for j in range(n)]) == s) for i in range(n)]
    [sol.add(Sum([x[(i, j)] for i in range(n)]) == s) for j in range(n)]

    sol.add(Sum([x[(i, i)] for i in range(n)]) == s)  # diag 1
    sol.add(Sum([x[(i, n - i - 1)] for i in range(n)]) == s)  # diag 2

    # symmetry breaking
    # sol.add(x[(0,0)] == 1)

    print(sol.check())
    end = time.clock()
    value = end - start
    print("Time to sat: ", value)

    if sol.check() == sat:
        mod = sol.model()
        print("s:", mod.eval(s))
        # print_square(mod,x,n)
    else:
        print("No solution found!")

    end = time.clock()
    value = end - start
    print("Time all: ", value)


for n in [3,4,5]:
    print("Testing ", n)
    magic_square(n)
    print()
