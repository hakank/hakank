#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Sicherman Dice problem in Z3
#
# From http://en.wikipedia.org/wiki/Sicherman_dice
# ""
# Sicherman dice are the only pair of 6-sided dice which are not normal dice,
# bear only positive integers, and have the same probability distribution for
# the sum as normal dice.
#
# The faces on the dice are numbered 1, 2, 2, 3, 3, 4 and 1, 3, 4, 5, 6, 8.
# ""
#
# I read about this problem in a book/column by Martin Gardner long
# time ago, and got inspired to model it now by the WolframBlog post
# "Sicherman Dice": http://blog.wolfram.com/2010/07/13/sicherman-dice/
#
# This model gets the two different ways, first the standard way and
# then the Sicherman dice:
#
# x1 = [1, 2, 3, 4, 5, 6]
# x2 = [1, 2, 3, 4, 5, 6]
# ----------
# x1 = [1, 2, 2, 3, 3, 4]
# x2 = [1, 3, 4, 5, 6, 8]
#
#
# Extra: If we also allow 0 (zero) as a valid value then the
# following two solutions are also valid:
#
# x1 = [0, 1, 1, 2, 2, 3]
# x2 = [2, 4, 5, 6, 7, 9]
# ----------
# x1 = [0, 1, 2, 3, 4, 5]
# x2 = [2, 3, 4, 5, 6, 7]
#
# These two extra cases are mentioned here:
# http://mathworld.wolfram.com/SichermanDice.html
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
from z3_utils_hakank import *


def main():

  sol = SimpleSolver()

  #
  # data
  #
  n = 6
  m = 10

  # standard distribution
  standard_dist = [1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 1]

  #
  # declare variables
  #

  # the two dice
  x1 = [makeIntVar(sol,"x1(%i)" % i, 0, m) for i in range(n)]
  x2 = [makeIntVar(sol,"x2(%i)" % i, 0, m) for i in range(n)]

  # constraints
  [sol.add(standard_dist[k] == Sum([If(x1[i] + x2[j] == k+2,1,0) for i in range(n) for j in range(n)]))
   for k in range(len(standard_dist))]

  # symmetry breaking
  [sol.add(x1[i] <= x1[i + 1]) for i in range(n - 1)],
  [sol.add(x2[i] <= x2[i + 1]) for i in range(n - 1)],
  [sol.add(x1[i] <= x2[i]) for i in range(n - 1)],

  # solution and search
  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print("x1:", [mod.eval(x1[i]) for i in range(n)])
    print("x2:", [mod.eval(x2[i]) for i in range(n)])
    print()
    getDifferentSolution(sol,mod,x1,x2)


  print()
  print("num_solutions:", num_solutions)

if __name__ == "__main__":
  main()
