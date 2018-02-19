#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Coin application in Z3
# From 'Constraint Logic Programming using ECLiPSe'
# pages 99f and 234 ff.
# The solution in ECLiPSe is at page 236.
#
# '''
# What is the minimum number of coins that allows one to pay _exactly_
# any amount smaller than one Euro? Recall that there are six different
# euro cents, of denomination 1, 2, 5, 10, 20, 50
# '''

# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
#
from __future__ import print_function
import sys
from z3_utils_hakank import *


def main(type="optimize"):
  if type == "optimize":
      sol = Optimize()
  else:
      # sol = Solver()
      sol = SolverFor("QF_LIA")

  # data
  n = 6  # number of different coins
  variables = [1, 2, 5, 10, 25, 50]

  # declare variables
  x = [makeIntVar(sol, "x%i" % i, 0, 99) for i in range(n)]
  num_coins = makeIntVar(sol, "num_coins", 0, 99)

  # constraints

  # number of used coins, to be minimized
  sol.add(num_coins == Sum([x[i] for i in range(n) ] ))

  # Check that all changes from 1 to 99 can be made.
  c = 0
  for j in range(1, 100):
    tmp = [makeIntVar(sol,"tmp%i_%i" % (i,c), 0, 99) for i in range(n)]
    sol.add(scalar_product2(sol,tmp, variables) == j)
    [sol.add(tmp[i] <= x[i]) for i in range(n)]
    c += 1

  # objective
  if type == "optimize":
      sol.minimize(num_coins)

  # solution and search
  if type == "optimize":
      if sol.check() == sat:
        mod = sol.model()    
        print("x: ", [mod.eval(x[i]) for i in range(n)])
        print("num_coins:", mod.eval(num_coins))
        print()
  else:
      num_solutions = 0
      while sol.check() == sat:
        num_solutions += 1
        mod = sol.model()    
        print("x: ", [mod.eval(x[i]) for i in range(n)])
        print("num_coins:", mod.eval(num_coins))
        # sol.add([x[i] != mod.eval(x[i]) for i in range(n)])
        sol.add(num_coins < mod.eval(num_coins))
        print()
      print("num_solutions:", num_solutions)
          

if __name__ == "__main__":
  # main("optimize")
  main("solver")
