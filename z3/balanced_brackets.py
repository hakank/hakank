#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Generate balanced brackets in Z3
#
# This model generates balanced brackets of size m*2.
#
# The number of generated solutions for m:
#
#  m        #
#  ----------
#   1       1
#   2       2
#   3       5
#   4      14
#   5      42
#   6     132
#   7     429
#   8    1430
#   9    4862
#  10   16796
#  11   58786
#  12  208012
#  13  742900
#
# 
# Which - of course - is the Catalan numbers.
#
# http://oeis.org/search?q=1#2C2#2C5#2C14#2C42#2C132#2C429#2C1430#2C4862#2C16796#2C58786#2C208012&language=english&go=Search
# http://oeis.org/A000108
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

def brackets(m,do_print=False):
    sol = SimpleSolver()
    n = m*2

    s = ["[","]"]

    # For cumulative (c):
    # +1 if x[i] = "["
    # -1 if x[i] = "]"
    # t = makeIntArray(sol,"t", 2,-1,1) # must be Array since we use element
    t = makeIntVector(sol,"t", 2,-1,1)
    sol.add(t[0] == 1)
    sol.add(t[1] == -1)

    # 0: "[", 1: "]"
    x = makeIntVector(sol,"x",n,0,1)
    c = makeIntVector(sol,"c",n,0,n) # counter (cumulative)
    

    # constraints
    sol.add(x[0] == 0)
    sol.add(c[0] == 1)

    # cumulative
    for i in range(1,n):
        # sol.add(c[i] == c[i-1] + t[x[i]])        
        txi = Int(f"tx{i}")
        element(sol,x[i],t,txi,2)
        sol.add(c[i] == c[i-1] + txi)

    sol.add(x[n-1] == 1)
    sol.add(c[n-1] == 0) # end

    # Redundant constraint: This might make it faster (but it don't)
    # sol.add(Sum(x) == m)


    num_solutions = 0
    while sol.check() == sat:
        num_solutions += 1
        mod = sol.model()
        if do_print:
            print("x:", [mod[x[i]] for i in range(n)])
            print("c:", [mod[c[i]] for i in range(n)])
            print("cc:", "".join([s[mod[x[i]].as_long()] for i in range(n)]))
            print()
        getDifferentSolution(sol,mod,x + c)

    print("m=%i: num_solutions: %i " % (m, num_solutions))

for i in range(1,11):
    brackets(i,False)
