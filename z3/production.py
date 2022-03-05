#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Production planning problem in Z3
#
# From the OPL model production.mod.
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
from z3_utils_hakank import *

def main():

  sol = Optimize() # SimpleSolver()

  # data

  kluski = 0
  capellini = 1
  fettucine = 2
  products = ['kluski', 'capellini', 'fettucine']
  num_products = len(products)

  flour = 0
  eggs = 1
  resources = ['flour', 'eggs']
  num_resources = len(resources)

  consumption = [[0.5, 0.2], [0.4, 0.4], [0.3, 0.6]]
  capacity = [20, 40]
  demand = [100, 200, 300]
  inside_cost = [0.6, 0.8, 0.3]
  outside_cost = [0.8, 0.9, 0.4]

  #
  # declare variables
  #
  inside = [makeRealVar(sol, 'inside[%i]' % p, 0, 10000)
            for p in range(num_products)]
  outside = [makeRealVar(sol, 'outside[%i]' % p, 0, 10000)
             for p in range(num_products)]

  # to minimize
  z = Real("z")
      

  #
  # constraints
  #
  sol.add(z == Sum([inside_cost[p] * inside[p] + outside_cost[p] * outside[p]
                    for p in range(num_products)]))

  for r in range(num_resources):
    sol.add(Sum(
      [consumption[p][r] * inside[p]
       for p in range(num_products)]) <= capacity[r])

  for p in range(num_products):
    sol.add(inside[p] + outside[p] >= demand[p])

  sol.minimize(z)

  num_solutions = 0
  if sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print('z = ', mod.eval(z).as_decimal(5))  
    for p in range(num_products):
        print(products[p], ': inside:', mod.eval(inside[p]), end=' ')
    print('outside:', mod.eval(outside[p]))
    print()
    getLessSolution(sol,mod,z)


if __name__ == '__main__':
  main()
