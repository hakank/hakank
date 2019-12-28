#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Dinner problem in Z3
#
# From http://www.sellsbrothers.com/spout/#The_Logic_of_Logic
# """
# My son came to me the other day and said, "Dad, I need help with a
# math problem." The problem went like this:
#
# * We're going out to dinner taking 1-6 grandparents, 1-10 parents and/or 1-40 children
# * Grandparents cost $3 for dinner, parents $2 and children $0.50
# * There must be 20 total people at dinner and it must cost $20
# * How many grandparents, parents and children are going to dinner?
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

n = 3

# variables
# x = makeIntVector(sol, "x", 3, 1, 100)
# x = makeRealVector(sol, "x", 3, 1, 100)
# Grandparents, Parents, Children = x

Grandparents = makeRealVar(sol,"Grandparents", 1,6)
Parents = makeRealVar(sol,"Parents", 1,10)
Children = makeRealVar(sol,"Children", 1,40)

# constraints
# 
# sol.add(3*Grandparents + 2*Parents + Children/2 == 20)

# multiply with 2:
sol.add(Grandparents * 6 + Parents * 4 + Children * 1  == 40)
#       Grandparents + Parents + Children = 20 /\ 
       

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print([mod.eval(x) for x in [Grandparents,Parents,Children]])
  getDifferentSolution(sol,mod,[Grandparents,Parents,Children])
  if num_solutions > 5:
      break;

print('num_solutions:', num_solutions)
