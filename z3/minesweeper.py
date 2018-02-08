#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Minesweeper problem in Z3
#
# From gecode/examples/minesweeper.cc:
# """
# A specification is a square matrix of characters. Alphanumeric
# characters represent the number of mines adjacent to that field.
# Dots represent fields with an unknown number of mines adjacent to
# it (or an actual mine).
# '''
# 
# E.g.
#      '..2.3.'
#      '2.....'
#      '..24.3'
#      '1.34..'
#      '.....3'
#      '.3.3..'
#
#   Also see:
#   * http://www.janko.at/Raetsel/Minesweeper/index.htm
# 
#   * http://en.wikipedia.org/wiki/Minesweeper_(computer_game)
#
#   * Ian Stewart on Minesweeper:
#     http://www.claymath.org/Popular_Lectures/Minesweeper/
#
#   * Richard Kaye's Minesweeper Pages
#     http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.htm
#
#   * Some Minesweeper Configurations
#     http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.pdf
# """
#
# This model is heavily based on my Google or-tool model: http://hakank.org/or-tools/minesweeper.py
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
# 
from z3_utils_hakank import *


#
# Default problem from "Some Minesweeper Configurations",page 3
# (it's the same as the instance file minesweeper_config3.txt)
# It has 4 solutions.
#
default_r = 8
default_c = 8
X = -1
default_game = [
    [2, 3, X, 2, 2, X, 2, 1],
    [X, X, 4, X, X, 4, X, 2],
    [X, X, X, X, X, X, 4, X],
    [X, 5, X, 6, X, X, X, 2],
    [2, X, X, X, 5, 5, X, 2],
    [1, 3, 4, X, X, X, 4, X],
    [0, 1, X, 4, X, X, X, 3],
    [0, 1, 2, X, 2, 3, X, 2]
]

def main(game="", r="", c=""):

  sol = Solver()

  #
  # data
  #

  # Set default problem
  if game == "":
    game = default_game
    r = default_r
    c = default_c
  else:
    print "rows:", r, " cols:", c

  S = [-1, 0, 1]  # for the neighbors of "this" cell

  # print problem instance
  print "Problem:"
  for i in range(r):
    for j in range(c):
      if game[i][j] == X:
        print "X", 
      else:
        print game[i][j],
    print
  print

  # declare variables
  mines = {}
  for i in range(r):
    for j in range(c):
        mines[(i, j)] = Int("mines %i %i" % (i, j))
        sol.add(mines[(i, j)] >= 0, mines[(i, j)] <= 1)

  # constraints
  for i in range(r):
    for j in range(c):
      if game[i][j] >= 0:
        sol.add(mines[i, j] == 0)
        # this cell is the sum of all the surrounding cells
        sol.add(
            game[i][j] == Sum([mines[i+a,j+b]
                                      for a in S for b in S
                                      if i + a >= 0 and
                                      j + b >= 0 and
                                      i + a < r and
                                      j + b < c])
        )
      if game[i][j] > X:
        # This cell cannot be a mine
        sol.add(mines[i, j] == 0)

  #
  # solution and search
  #
  # solution = solver.Assignment()
  # solution.Add([mines[(i, j)] for i in range(r) for j in range(c)])

  num_solutions = 0
  print "Solution(s):"
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    for i in range(r):
        for j in range(c):
            print mod.eval(mines[(i,j)]),
        print
    print
    # sol.add(Or([mines[(i,j)] != mod.eval(mines[(i,j)]) for i in range(r) for j in range(c)]))
    getDifferentSolutionMatrix(sol,mod,mines,r,c)
  print "num_solutions:", num_solutions    

#
# Read a problem instance from a file
#
def read_problem(file):
  f = open(file, "r")
  rows = int(f.readline())
  cols = int(f.readline())
  game = []
  for i in range(rows):
    x = f.readline()
    row = [0] * cols
    for j in range(cols):
      if x[j] == ".":
        tmp = -1
      else:
        tmp = int(x[j])
      row[j] = tmp
    game.append(row)
  return [game, rows, cols]


#
# Print the mines
#
def print_mines(mines, rows, cols):
  for i in range(rows):
    for j in range(cols):
      print mines[i, j],
    print


def print_game(game, rows, cols):
  for i in range(rows):
    for j in range(cols):
      print game[i][j],
    print


if __name__ == "__main__":
  if len(sys.argv) > 1:
    file = sys.argv[1]
    print "Problem instance from", file
    [game, rows, cols] = read_problem(file)
    # print_game(game, rows, cols)
    main(game, rows, cols)
  else:
    main()
