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
import time
from z3_utils_hakank import *

#
# "Traditional" encoding
# 0.1158s, slightly faster
#
def bales_of_hay1():

  sol = SolverFor("QF_LIA") # SimpleSolver()

  n = 5
  weights =  [80, 82, 83, 84, 85, 86, 87, 88, 90, 91]

  # variables
  bales = makeIntVector(sol,"bales",n,0,50)
  
  # constraints
  increasing(sol,[bales[i] for i in range(n)])
  
  cc = 0
  for w in range(10):
    ii = makeIntVar(sol,"ii_%i_%i" % (w,cc), 0,n-1)
    jj = makeIntVar(sol,"jj_%i_%i" % (w,cc), 0,n-1)  
    sol.add(ii < jj)
    bales_ii = Int(f"bales_ii{w}")
    bales_jj = Int(f"bales_jj{w}")
    element(sol,ii,bales,bales_ii,n)
    element(sol,jj,bales,bales_jj,n)
    sol.add(bales_ii + bales_jj == weights[w])  
    cc += 1

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print([mod.eval(bales[w]) for w in range(n)])
    getDifferentSolution(sol,mod,[bales[i] for i in range(n)])

  print("num_solutions:", num_solutions)

#
# Using Function instead of lists.
# 0.1196s, slightly slower.
#
def bales_of_hay2():

  sol = SolverFor("AUFLIA") # 0.11826205253601074  
  
  n = 5
  weights =  [80, 82, 83, 84, 85, 86, 87, 88, 90, 91]

  # variables
  bales = Function(f"bales",IntSort(), IntSort())
  # makeIntVector(sol,"bales",n,0,50)
  
  # constraints
  increasing(sol,[bales(i) for i in range(n)])
  
  cc = 0
  for w in range(10):
    i = Int(f"i[{w}]")
    sol.add(i >= 0, i <= n-1) 
    j = Int(f"j[{w}]")
    sol.add(j >= 0, j <= n-1) 
    sol.add(i < j)
    sol.add(bales(i) + bales(j) == weights[w])  
    cc += 1

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print([mod.eval(bales(w)) for w in range(n)])
    sol.add(Or([mod.eval(bales(w)) != bales(w) for w in range(n)]))

  print("num_solutions:", num_solutions)


  
t0 = time.time()
bales_of_hay1()
t1 = time.time()
print("Time:", t1-t0)

bales_of_hay2()
t2 = time.time()
print("Time:", t2-t1)


