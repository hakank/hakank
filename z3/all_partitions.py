#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# All partitions in Z3
#
# Simple implementation of all partitions.
#
# For the number of different partitions, see
# The On-Line Encyclopedia of Integer Sequences:
# http://www.research.att.com/~njas/sequences/A000041
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

#
# The first part is simply to get an ordered array where the sum is N.
# This ordering is needed for removing symmetries when all
# 0's is remove in the second part.
# 
def all_partitions(n):

    sol = Solver()
    
    # part I: get all candidates
    x = makeIntVector(sol,"x",n,0,n)
    sol.add(n == Sum(x))
    increasing(sol, x)

    p = []
    while sol.check() == sat:
        mod = sol.model()
        xx = [mod[x[i]].as_long() for i in range(n)]
        # part II: remove all 0's from X
        p.append([i for i in xx if i != 0])
        getDifferentSolution(sol,mod,x)

    p.sort()
    return p


for n in range(21):
    pp = all_partitions(n)
    ll = len(pp)
    print("n:", n, "len:", ll)
    if ll < 20:
        print(pp)
        print()
