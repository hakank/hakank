#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Bales of hay problem in Z3
#
# From The Math Less Traveled, 
# "The haybaler", http://www.mathlesstraveled.com/?p=582 
# """
# You have five bales of hay.
#
# For some reason, instead of being weighed individually, they were weighed 
# in all possible combinations of two. The weights of each of these 
# combinations were written down and arranged in numerical order, without 
# keeping track of which weight matched which pair of bales. The weights, 
# in kilograms, were 80, 82, 83, 84, 85, 86, 87, 88, 90, and 91.
#
# How much does each bale weigh? Is there a solution? Are there multiple 
# possible solutions? 
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

n = 5
weights =  [80, 82, 83, 84, 85, 86, 87, 88, 90, 91]

# variables
bales = makeIntArray(sol,"bales",n,0,50)

# constraints
increasing(sol,[bales[i] for i in range(n)])

cc = 0
for w in range(10):
  ii = makeIntVar(sol,"ii_%i_%i" % (w,cc), 0,n-1)
  jj = makeIntVar(sol,"jj_%i_%i" % (w,cc), 0,n-1)  
  sol.add(ii < jj)
  sol.add(bales[ii] + bales[jj] == weights[w])
  cc += 1

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print([mod.eval(bales[w]) for w in range(n)])
  getDifferentSolution(sol,mod,[bales[i] for i in range(n)])

print("num_solutions:", num_solutions)

