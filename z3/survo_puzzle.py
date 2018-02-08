#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Survo puzzle in Z3
#
# http://en.wikipedia.org/wiki/Survo_Puzzle
# '''
# Survo puzzle is a kind of logic puzzle presented (in April 2006) and studied
# by Seppo Mustonen. The name of the puzzle is associated to Mustonen's
# Survo system which is a general environment for statistical computing and
# related areas.
#
# In a Survo puzzle the task is to fill an m * n table by integers 1,2,...,m*n
# so
# that each of these numbers appears only once and their row and column sums are
# equal to integers given on the bottom and the right side of the table.
# Often some of the integers are given readily in the table in order to
# guarantee uniqueness of the solution and/or for making the task easier.
# '''
#
# See also
# http://www.survo.fi/english/index.html
# http://www.survo.fi/puzzles/index.html
#
# References:
# Mustonen, S. (2006b). "On certain cross sum puzzles"
# http://www.survo.fi/papers/puzzles.pdf
# Mustonen, S. (2007b). "Enumeration of uniquely solvable open Survo puzzles."
# http://www.survo.fi/papers/enum_survo_puzzles.pdf
# Kimmo Vehkalahti: "Some comments on magic squares and Survo puzzles"
# http://www.helsinki.fi/~kvehkala/Kimmo_Vehkalahti_Windsor.pdf
# R code: http://koti.mbnet.fi/tuimala/tiedostot/survo.R
#
# This Z3 model was inspired by my Google
#
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
# 
from z3_utils_hakank import *


def survo_puzzle(r=0, c=0, rowsums=[], colsums=[], game=[]):

  sol = Solver()

  # data
  if r == 0:
    r = 3
    c = 4
    rowsums = [30, 18, 30]
    colsums = [27, 16, 10, 25]
    game = [[0, 6, 0, 0],
            [8, 0, 0, 0],
            [0, 0, 3, 0]]

  print "r:", r, "c:", c

  # declare variables
  x = {}
  for i in range(r):
    for j in range(c):
      x[(i,j)] = Int("x %i %i" % (i,j))
      sol.add(x[(i,j)] >= 1, x[(i,j)] <= r*c)

  # constraints

  # set the clues
  for i in range(r):
    for j in range(c):
      if game[i][j] > 0:
        sol.add(x[i, j] == game[i][j])

  xflat = [x[(i, j)] for i in range(r) for j in range(c)]
  sol.add(Distinct(xflat))

  # calculate rowsums and colsums
  for i in range(r):
    sol.add(rowsums[i] == Sum([x[i, j] for j in range(c)]))

  for j in range(c):
    sol.add(colsums[j] == Sum([x[i, j] for i in range(r)]))

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    xx = {}
    for i in range(r):
        for j in range(c):
          xx[(i,j)] = mod.eval(x[i, j])
          print "%2s" % (xx[i, j]),
        print
    print
    sol.add(Or([x[(i, j)] != xx[(i, j)] for i in range(r) for j in range(c)]))
    
  print "num_solutions:", num_solutions


#
# Read a problem instance from a file
#
def read_problem(file):
  f = open(file, "r")
  r = int(f.readline())
  c = int(f.readline())
  rowsums = f.readline()
  colsums = f.readline()
  rowsums = [int(t) for t in (rowsums.rstrip()).split(",")]
  colsums = [int(t) for t in (colsums.rstrip()).split(",")]
  game = []
  for i in range(r):
    x = f.readline()
    x = [int(t) for t in (x.rstrip()).split(",")]
    row = [0] * c
    for j in range(c):
      row[j] = int(x[j])
    game.append(row)
  return [r, c, rowsums, colsums, game]


if __name__ == "__main__":
  if len(sys.argv) > 1:
    file = sys.argv[1]
    [r, c, rowsums, colsums, game] = read_problem(file)
    survo_puzzle(r, c, rowsums, colsums, game)
  else:
    survo_puzzle()

