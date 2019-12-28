#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Global constraint global_contiguity in Z3
#
# From Global Constraint Catalogue
# http://www.emn.fr/x-info/sdemasse/gccat/Cglobal_contiguity.html
# """
# Enforce all variables of the VARIABLES collection to be assigned to 0 or 1. 
# In addition, all variables assigned to value 1 appear contiguously.
# 
# Example:
# (<0,1,1,0>)
# 
# The global_contiguity constraint holds since the sequence 0 1 1 0 contains 
# no more than one group of contiguous 1.
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()
n = 4
x = makeIntVector(sol,"x",n,0,1)
start = makeIntVar(sol,"start",0,n-1)
end = makeIntVar(sol,"end",0,n-1)

global_contiguity(sol,x,start,end)

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print("[start,end]:", [mod.eval(start),mod.eval(end)])
  print([mod.eval(x[i]) for i in range(n)])
  getDifferentSolution(sol,mod,x,[start,end])

print("num_solutions:", num_solutions)
