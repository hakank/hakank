#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# global constraint inverse (decomposition) in Z3
#
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
# 
from z3_utils_hakank import *

sol = Solver()
n = 6
x = IntVector("x",n)
y = IntVector("y",n)
# Note: 0 based!
# for i in range(n):
#     sol.add(x[i] >= 0, x[i] <= n-1)
#     sol.add(y[i] >= 0, y[i] <= n-1)
# Alternative 
sol.add([And(x[i] >= 0, x[i] <= n-1,y[i] >= 0, y[i] <= n-1) for i in range(n)])    

## These are not needed since the inverse constraint enforces this
## It's a bit slower with these constraints
# sol.add(Distinct([x[i] for i in range(n)]))
# sol.add(Distinct([y[i] for i in range(n)]))

inverse(sol, x,y, n) # 720 solutions for n = 6

# self-inverse (a.k.a assignment)
# inverse(sol, x,x, n) # 76 solutions for n = 6

print sol.check()
num_solutions = 0
while sol.check() == sat:
  mod = sol.model()
  num_solutions += 1
  xx = [mod.eval(x[i]) for i in range(n)]
  yy = [mod.eval(y[i]) for i in range(n)]  
  print "x:", xx
  print "y:", yy
  print
  sol.add(Or([xx[i] != x[i] for i in range(n)]))

print "num_solutions:", num_solutions


