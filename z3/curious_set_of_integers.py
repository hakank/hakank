#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Curious set of integers problem in Z3
#
# Martin Gardner (February 1967):
# """
# The integers 1,3,8, and 120 form a set with a remarkable priperty: the product of any two integers is one less than a perfect square. Find a fifth number that can be added to the set without destroying this property.
# """
# 
# Solution: The number is 0.
#
# There are however other sets of five numbers with this property.
# Here are the one in the range of 0.10000:
# [0, 1, 3, 8, 120]
# [0, 1, 3, 120, 1680]
# [0, 1, 8, 15, 528]
# [0, 1, 8, 120, 4095]
# [0, 1, 15, 24, 1520]
# [0, 1, 24, 35, 3480]
# [0, 1, 35, 48, 6888]
# [0, 2, 4, 12, 420]
# [0, 2, 12, 24, 2380]
# [0, 2, 24, 40, 7812]
# [0, 3, 5, 16, 1008]
# [0, 3, 8, 21, 2080]
# [0, 3, 16, 33, 6440]
# [0, 4, 6, 20, 1980]
# [0, 4, 12, 30, 5852]
# [0, 5, 7, 24, 3432]
# [0, 6, 8, 28, 5460]
# [0, 7, 9, 32, 8160]
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

n = 5

# variables
x = makeIntVector(sol, "x", n, 0, 10000)

# constraints
sol.add(Distinct([x[i] for i in range(n)]))
increasing(sol, x)

for i in range(n):
  for j in range(n):
    if i != j:
      p = makeIntVar(sol, "p_%i_%i" % (i,j), 0,10000)
      sol.add(p*p-1 == x[i]*x[j])


sol.add(Or(
     And(x[0] < 1, x[1] == 1 , x[2] == 3 , x[3] == 8 , x[4] == 120)
     ,
     And(x[0] == 1, x[1] == 3 , x[2] == 8 , x[3] == 120 , x[4] > 120)
   ))

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print("x:", [mod.eval(x[i]) for i in range(n)])
  getDifferentSolution(sol,mod,x)

print("num_solutions:", num_solutions)

