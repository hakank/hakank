#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Map coloring problem in Z3
#
# From Pascal Van Hentenryck 'The OPL Optimization Programming Language',
# page 7, 42. Map coloring of some European countries.
# 
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
#
from __future__ import print_function
from z3_utils_hakank import *


def main():

  sol = SolverFor("QF_FD")

  #
  # data
  #
  Belgium = 0
  Denmark = 1
  France = 2
  Germany = 3
  Netherlands = 4
  Luxembourg = 5

  n = 6
  max_num_colors = 4

  # declare variables
  color = makeIntVector(sol,"color", n, 1,max_num_colors)

  #
  # constraints
  #
  sol.add(color[Belgium] == 1)  # Symmetry breaking
  sol.add(color[France] != color[Belgium])
  sol.add(color[France] != color[Luxembourg])
  sol.add(color[France] != color[Germany])
  sol.add(color[Luxembourg] != color[Germany])
  sol.add(color[Luxembourg] != color[Belgium])
  sol.add(color[Belgium] != color[Netherlands])
  sol.add(color[Belgium] != color[Germany])
  sol.add(color[Germany] != color[Netherlands])
  sol.add(color[Germany] != color[Denmark])

  num_solutions = 0
  while sol.check() == sat:
      num_solutions += 1
      mod = sol.model()
      colorval = [mod.eval(color[i]) for i in range(n)]
      print("color:", colorval)
      sol.add(Or([color[i] != mod.eval(color[i]) for i in range(n)]))

  print()
  print("num_solutions:", num_solutions)


if __name__ == "__main__":
  main()


