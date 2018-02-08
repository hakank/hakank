#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Quasigroup completion in Z3
#
# See Carla P. Gomes and David Shmoys:
# "Completing Quasigroups or Latin Squares: Structured Graph Coloring Problem"
#
# See also
# Ivars Peterson "Completing Latin Squares"
# http://www.maa.org/mathland/mathtrek_5_8_00.html
# '''
#   Using only the numbers 1, 2, 3, and 4, arrange four sets of these numbers
#   into
#   a four-by-four array so that no column or row contains the same two numbers.
#   The result is known as a Latin square.
#   ...
#   The so-called quasigroup completion problem concerns a table that is
#   correctly
#   but only partially filled in. The question is whether the remaining blanks
#   in
#   the table can be filled in to obtain a complete Latin square (or a proper
#   quasigroup multiplication table).
#   '''

# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
#
from __future__ import print_function
import sys
from z3_utils_hakank import *



default_n = 5
X = 0
# default problem
# (This is the same as quasigroup1.txt)
default_puzzle = [
    [1, X, X, X, 4],
    [X, 5, X, X, X],
    [4, X, X, 2, X],
    [X, 4, X, X, X],
    [X, X, 5, X, 1]
]


def quasigroup_completion(puzzle="", n=0):

  sol = Solver()

  # data
  if puzzle == "":
    puzzle = default_puzzle
    n = default_n

  print("Problem:")
  print_game(puzzle, n, n)

  # declare variables
  x = {}
  for i in range(n):
    for j in range(n):
      x[(i, j)] = Int("x %i %i" % (i, j))
      sol.add(x[(i,j)] >= 1, x[(i,j)] <= n)

  xflat = [x[(i, j)] for i in range(n) for j in range(n)]

  # constraints

  # set the clues
  for i in range(n):
    for j in range(n):
      if puzzle[i][j] > X:
        sol.add(x[i, j] == puzzle[i][j])

  # rows and columns must be different
  for i in range(n):
    sol.add(Distinct([x[i, j] for j in range(n)]))
    sol.add(Distinct([x[j, i] for j in range(n)]))


  # solution and search
  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print("Solution %i" % num_solutions)
    xval = [mod.eval(x[(i, j)]) for i in range(n) for j in range(n)]
    for i in range(n):
      for j in range(n):
        print(xval[i * n + j], end=' ')
      print()
    print()
    getDifferentSolutionMatrix(sol,mod,x,n,n)

  print("num_solutions:", num_solutions)



#
# Read a problem instance from a file
#
def read_problem(file):
  f = open(file, "r")
  n = int(f.readline())
  game = []
  for i in range(n):
    x = f.readline()
    row_x = (x.rstrip()).split(" ")
    row = [0] * n
    for j in range(n):
      if row_x[j] == ".":
        tmp = 0
      else:
        tmp = int(row_x[j])
      row[j] = tmp
    game.append(row)
  return [game, n]


def print_board(x, rows, cols):
  for i in range(rows):
    for j in range(cols):
      print("% 2s" % x[i, j], end=' ')
    print("")


def print_game(game, rows, cols):
  for i in range(rows):
    for j in range(cols):
      print("% 2s" % game[i][j], end=' ')
    print("")


if __name__ == "__main__":

  if len(sys.argv) > 1:
    file = sys.argv[1]
    print("Problem instance from", file)
    [game, n] = read_problem(file)
    quasigroup_completion(game, n)
  else:
    quasigroup_completion()
