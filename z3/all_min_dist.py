#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Global constraint all_min_dist in Z3
#
# From Global Constraint Catalogue
# http://www.emn.fr/x-info/sdemasse/gccat/Call_min_dist.html
# """
# Enforce for each pair (vari, varj) of distinct variables of the 
# collection VARIABLES that 
# |vari - varj| >= MINDIST.
#
# Example
#  (2, <5, 1, 9, 3>)
#
# The all_min_dist constraint holds since the following expressions 
# |5-1|, |5-9|, |5-3|, |1-9|, |1-3|, |9-3| are all greater than or equal 
# to the first argument MINDIST = 2 of the all_min_dist constraint.
# """

# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

n = 4

# variables
x = makeIntVector(sol,"x", n, 1,9)
c = 2 # min dist

# constraints

all_min_dist(sol,c,x,n)
increasing(sol,x) # symmetry breaking

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print "x  :", [mod.eval(x[i]) for i in range(n)]
  print
  getDifferentSolution(sol,mod,x)

print "num_solutions:", num_solutions  
