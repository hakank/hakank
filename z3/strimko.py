#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Strimko problem in Z3
# 
# From
# 360: A New Twist on Latin Squares
# http://threesixty360.wordpress.com/2009/08/04/a-new-twist-on-latin-squares/
# '''
# The idea is simple: each row and column of an nxn grid must contain
# the number 1, 2, ... n exactly once (that is, the grid must form a
# Latin square), and each "stream" (connected path in the grid) must
# also contain the numbers 1, 2, ..., n exactly once.
# '''
#
# For more information, see:
# * http://www.strimko.com/
# * http://www.strimko.com/rules.htm
# * http://www.strimko.com/about.htm
# * http://www.puzzlersparadise.com/Strimko.htm
# 
# I blogged about this (using a MiniZinc model) in
# 'Strimko - Latin squares puzzle with "streams"'
# http://www.hakank.org/constraint_programming_blog/2009/08/strimko_latin_squares_puzzle_w_1.html
#
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
#
from __future__ import print_function
import sys
from z3_utils_hakank import *


def main(streams='', placed=''):

  # Create the solver.
  sol = Solver()

  #
  # default problem
  #
  if streams == '':
    streams = [
        [1, 1, 2, 2, 2, 2, 2],
        [1, 1, 2, 3, 3, 3, 2],
        [1, 4, 1, 3, 3, 5, 5],
        [4, 4, 3, 1, 3, 5, 5],
        [4, 6, 6, 6, 7, 7, 5],
        [6, 4, 6, 4, 5, 5, 7],
        [6, 6, 4, 7, 7, 7, 7]]

    # Note: This is 1-based
    placed = [
        [2, 1, 1],
        [2, 3, 7],
        [2, 5, 6],
        [2, 7, 4],
        [3, 2, 7],
        [3, 6, 1],
        [4, 1, 4],
        [4, 7, 5],
        [5, 2, 2],
        [5, 6, 6]]

  n = len(streams)
  num_placed = len(placed)

  print('n:', n)

  #
  # variables
  #

  x = {}
  for i in range(n):
    for j in range(n):
      x[i, j] = makeIntVar(sol,'x[%i,%i]' % (i, j),1, n)

  x_flat = [x[i, j] for i in range(n) for j in range(n)]

  #
  # constraints
  #

  # all rows and columns must be unique, i.e. a Latin Square
  for i in range(n):
    sol.add(Distinct([x[i, j] for j in range(n)]))
    sol.add(Distinct([x[j, i] for j in range(n)]))

  #
  # streams
  #
  for s in range(1, n + 1):
    sol.add(Distinct([x[i, j] for i in range(n) for j in range(n) if streams[i][j] == s]))

  #
  # placed
  #
  for i in range(num_placed):
    # note: also adjust to 0-based
    sol.add(x[placed[i][0] - 1, placed[i][1] - 1] == placed[i][2])

  # search and solution
  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    for i in range(n):
      for j in range(n):
        print(mod.eval(x[i, j]), end=' ')
      print()
    sol.add(Or([x[i,j] != mod.eval(x[i,j])  ]))
    print()
    
  print()
  print('num_solutions:', num_solutions)

if __name__ == '__main__':
  if len(sys.argv) > 1:
    problem_file = sys.argv[1]
    exec(compile(open(problem_file).read(), problem_file, 'exec'))
    main(streams, placed)
  else:
    main()
