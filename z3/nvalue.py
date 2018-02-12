#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Global constraint nvalue in Z3
# """
# Clobal Constraint Catalog
# http://www.emn.fr/x-info/sdemasse/gccat/Cnvalue.html
# """
# Purpose 
#    NVAL is the number of distinct values taken by the variables of the collection VARIABLES
# Example
#   (4,<3,1,7,1,6>)
#
# The nvalue constraint holds since its first argument NVAL=4 is set to the number of distinct
# values occurring within the collection <3,1,7,1,6>.
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

n = 5
# m = 4 # number of distinct values
min_val = 1
max_val = n

# variables

# how many distinct values are there?
m = makeIntVar(sol,"m",0,n) 

x = makeIntVector(sol,"x",n, 1,n)

# constraints

nvalue(sol,m,x,min_val, max_val)

# sol.add(m==n)

# solutions

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print "m:", mod.eval(m)
  print "x:", [mod.eval(x[i]) for i in range(n)]
  print
  getDifferentSolution(sol,mod,x)

print "num_solutions:", num_solutions  





