#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# SEND + MORE = MONEY in z3
#
# Solve the alphametic equation: SEND + MORE = MONEY
# where all letters stands for a distinct digit (0..9)
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

s,e,n,d,m,o,r,y = Ints("s e n d m o r y")
t = [s,e,n,d,m,o,r,y]
for x in t:
    sol.add(x >= 0),
    sol.add(x <= 9)

sol.add(Distinct(t))
sol.add(1000*s + 100*e + 10*n + d  +  1000*m + 100*o + 10*r + e  == 10000*m + 1000*o + 100*n + 10*e + y )

sol.add(s > 0)
sol.add(m > 0)

if sol.check() == sat:
    mod = sol.model()
    print [mod.eval(x) for x in t]
else:
    print("failed to solve")


# if __name__ == '__main__':
#        pass

