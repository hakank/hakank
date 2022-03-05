#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Global constraint among in Z3
# """
# Requires exactly m variables in x to take one of the values in v.
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *


sol = SimpleSolver()

# data

n = 5 # length of x
m = 3 # number of values
v = [1,5,8]

# variables

x = makeIntVector(sol,"x",n,1,8)

# constraints

among(sol,m, x,v)


num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print("x :", [mod.eval(x[i]) for i in range(n)])
  getDifferentSolution(sol,mod,x)

print("num_solutions:", num_solutions)
