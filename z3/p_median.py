#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# P-median problem in Z3
#
# Model and data from the OPL Manual, which describes the problem:
# '''
# The P-Median problem is a well known problem in Operations Research.
# The problem can be stated very simply, like this: given a set of customers
# with known amounts of demand, a set of candidate locations for warehouses,
# and the distance between each pair of customer-warehouse, choose P
# warehouses to open that minimize the demand-weighted distance of serving
# all customers from those P warehouses.
# '''

# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
from z3_utils_hakank import *


def main():

  sol = Solver()

  #
  # data
  #
  p = 2

  num_customers = 4
  customers = list(range(num_customers))
  Albert, Bob, Chris, Daniel = customers
  num_warehouses = 3
  warehouses = list(range(num_warehouses))
  Santa_Clara, San_Jose, Berkeley = warehouses

  demand = [100, 80, 80, 70]
  distance = [
      [2, 10, 50],
      [2, 10, 52],
      [50, 60, 3],
      [40, 60, 1]
  ]

  #
  # declare variables
  #
  open = [makeIntVar(sol, "open[%i]" % w, 0,1) for w in warehouses]
  ship = {}
  for c in customers:
    for w in warehouses:
      ship[c, w] = makeIntVar(sol, "ship[%i,%i]" % (c, w), 0, 1)
  ship_flat = [ship[c, w]
               for c in customers
               for w in warehouses]

  z = makeIntVar(sol,"z", 0, 1000)

  #
  # constraints
  #
  sol.add(z == Sum([demand[c] * distance[c][w] * ship[c, w]
                    for c in customers
                    for w in warehouses]))

  for c in customers:
    sol.add(sum([ship[c, w]
                    for w in warehouses]) == 1)

  sol.add(Sum(open) == p)

  for c in customers:
    for w in warehouses:
      sol.add(ship[c, w] <= open[w])

  # objective
  # sol.minimize(z)

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print('z:', mod.eval(z))
    print('open:', [mod.eval(open[w]) for w in warehouses])
    for c in customers:
      for w in warehouses:
        print(mod.eval(ship[c, w]), end=' ')
      print()
    print()
    getLessSolution(sol,mod,z)      

  print('num_solutions:', num_solutions)


if __name__ == '__main__':
  main()
