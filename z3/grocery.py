#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Grocery problem (aka 7/11 puzzle) in Z3
#
# From  Christian Schulte, Gert Smolka, Finite Domain
# http://www.mozart-oz.org/documentation/fdt/
# Constraint Programming in Oz. A Tutorial. 2001.
# '''
# A kid goes into a grocery store and buys four items. The cashier
# charges $7.11, the kid pays and is about to leave when the cashier
# calls the kid back, and says 'Hold on, I multiplied the four items
# instead of adding them; I'll try again; Hah, with adding them the
# price still comes to $7.11'. What were the prices of the four items?
# '''

# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
from z3_utils_hakank import *


def main():

  sol = SolverFor("QF_FD")

  #
  # data
  #
  n = 4
  c = 711

  #
  # declare variables
  #
  item = [makeIntVar(sol, "item[%i]" % i, 0, c) for i in range(n)]

  #
  # constraints
  #
  sol.add(Sum(item) == c)
  sol.add(Product(item) == c * 100 ** 3)

  # symmetry breaking
  increasing(sol,item)

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print("item:", [mod.eval(item[i]) for i in range(n)])
    print()
    getDifferentSolution(sol,mod, item)


  print()
  print("num_solutions:", num_solutions)

if __name__ == "__main__":
  main()

