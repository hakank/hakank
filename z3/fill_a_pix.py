#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Fill-a-Pix problem in Z3
# 
# From
# http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/basiclogic
# '''
# Each puzzle consists of a grid containing clues in various places. The
# object is to reveal a hidden picture by painting the squares around each
# clue so that the number of painted squares, including the square with
# the clue, matches the value of the clue.
# '''
#
# http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/rules
# '''
# Fill-a-Pix is a Minesweeper-like puzzle based on a grid with a pixilated
# picture hidden inside. Using logic alone, the solver determines which
# squares are painted and which should remain empty until the hidden picture
# is completely exposed.
# '''
#
# Fill-a-pix History:
# http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/history
#
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
from z3_utils_hakank import *

# Puzzle 1 from
# http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/rules
default_n = 10
X = -1
default_puzzle = [
    [X, X, X, X, X, X, X, X, 0, X],
    [X, 8, 8, X, 2, X, 0, X, X, X],
    [5, X, 8, X, X, X, X, X, X, X],
    [X, X, X, X, X, 2, X, X, X, 2],
    [1, X, X, X, 4, 5, 6, X, X, X],
    [X, 0, X, X, X, 7, 9, X, X, 6],
    [X, X, X, 6, X, X, 9, X, X, 6],
    [X, X, 6, 6, 8, 7, 8, 7, X, 5],
    [X, 4, X, 6, 6, 6, X, 6, X, 4],
    [X, X, X, X, X, X, 3, X, X, X]
]


def main(puzzle='', n=''):

  sol = Solver()

  # data

  # Set default problem
  if puzzle == '':
    puzzle = default_puzzle
    n = default_n
  else:
    print('n:', n)

  # for the neighbors of 'this' cell
  S = [-1, 0, 1]

  # print problem instance
  print('Problem:')
  for i in range(n):
    for j in range(n):
      if puzzle[i][j] == X:
        sys.stdout.write('.')
      else:
        sys.stdout.write(str(puzzle[i][j]))
    print()
  print()

  #
  # declare variables
  #
  pict = {}
  for i in range(n):
    for j in range(n):
      pict[(i, j)] = makeIntVar(sol, 'pict %i %i' % (i, j), 0, 1)

  pict_flat = [pict[i, j] for i in range(n) for j in range(n)]

  #
  # constraints
  #
  for i in range(n):
    for j in range(n):
      if puzzle[i][j] > X:
        # this cell is the sum of all the surrounding cells
        sol.add(
            puzzle[i][j] == Sum([pict[i + a, j + b]
                                        for a in S for b in S
                                        if i + a >= 0 and
                                        j + b >= 0 and
                                        i + a < n and
                                        j + b < n])
        )

  num_solutions = 0
  print('Solution:')
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    for i in range(n):
      row = [str(mod.eval(pict[i, j])) for j in range(n)]
      for j in range(n):
        if row[j] == '0':
          row[j] = ' '
        else:
          row[j] = '#'
      print(''.join(row))
    print()
    getDifferentSolutionMatrix(sol,mod,pict,n,n)

  print('num_solutions:', num_solutions)


#
# Read a problem instance from a file
#
def read_problem(file):
  f = open(file, 'r')
  n = int(f.readline())
  puzzle = []
  for i in range(n):
    x = f.readline()
    row = [0] * n
    for j in range(n):
      if x[j] == '.':
        tmp = -1
      else:
        tmp = int(x[j])
      row[j] = tmp
    puzzle.append(row)
  return [puzzle, n]


if __name__ == '__main__':
  if len(sys.argv) > 1:
    file = sys.argv[1]
    print('Problem instance from', file)
    [puzzle, n] = read_problem(file)
    main(puzzle, n)
  else:
    main()
