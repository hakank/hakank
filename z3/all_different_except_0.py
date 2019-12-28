#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# all_different_except_0 constraint in Z3
#
# A decomposition of all_different_except_0/1

# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
# 
from z3_utils_hakank import *

sol = Solver()
n = 5
x = makeIntVector(sol,"x",n,0,n)

# sol.add(Distinct(x))
all_different_except_0(sol,x)
# all_different(sol,x)

count = 0
while sol.check() == sat:
    count = count + 1
    mod = sol.model()
    ss = [mod.eval(x[i]) for i in range(n)]
    print(ss)
    getDifferentSolution(sol,mod,x)

print("count:", count)
    
    



    
