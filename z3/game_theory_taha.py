#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Game theory in Z3
#
# 2 player zero sum game.
#
# From Taha, Operations Research (8'th edition), page 528.
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
from z3_utils_hakank import *


def main():

  # Create the solver.
  sol = Optimize()

  # data
  rows = 3
  cols = 3

  game = [[3.0, -1.0, -3.0],
          [-2.0, 4.0, -1.0],
          [-5.0, -6.0, 2.0]]

  #
  # declare variables
  #

  #
  # row player
  #
  x1 = [makeRealVar(sol, 'x1[%i]' % i, 0, 1)
        for i in range(rows)]

  v = makeRealVar(sol, 'v', -2, 2)

  for i in range(rows):
    sol.add(v - Sum([x1[j] * game[j][i] for j in range(cols)]) <= 0)

  sol.add(Sum(x1) == 1)

  sol.maximize(v)

  if sol.check() == sat:
    mod = sol.model()
    print()
    print('row player:')
    print('v = ', mod.eval(v).as_decimal(6))
    print('Strategies: ')
    for i in range(rows):
      print(mod.eval(x1[i]).as_decimal(6), end=' ')
    print()
    print()

  #
  # For column player:
  #
  x2 = [makeRealVar(sol, 'x2[%i]' % i, 0, 1)
        for i in range(cols)]

  v2 = makeRealVar(sol, 'v2', -2, 2)

  for i in range(cols):
    sol.add(v2 - Sum([x2[j] * game[i][j] for j in range(rows)]) >= 0)

  sol.add(Sum(x2) == 1)
  
  sol.minimize(v2)

  if sol.check() == sat:
      mod = sol.model()
      print()
      print('column player:')
      print('v2 = ', mod.eval(v2).as_decimal(6))
      print('Strategies: ')
      for i in range(rows):
        print(mod.eval(x2[i]).as_decimal(6), end=' ')
      print()

  print()

if __name__ == '__main__':
  main()
