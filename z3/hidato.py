#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Hidato puzzle in Z3
# http://www.shockwave.com/gamelanding/hidato.jsp
# http://www.hidato.com/
# '''
# Puzzles start semi-filled with numbered tiles.
# The first and last numbers are circled.
# Connect the numbers together to win. Consecutive
# number must touch horizontally, vertically, or
# diagonally.
# '''
#
# Note: This model is VERY slow, except for the simple instances.
# See hidato_table.py for a much faster model.
#
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from __future__ import print_function
from z3_utils_hakank import *


def main():

  sol = Solver()

  #
  # data
  #
  #
  # Simple problem
  #
  # r = 3
  # c = r
  # puzzle = [
  #     [6,0,9],
  #     [0,2,8],
  #     [1,0,0]
  #     ]


#     r = 7
#     c = 7
#     puzzle =  [
#         [0,44,41, 0, 0, 0, 0],
#         [0,43, 0,28,29, 0, 0],
#         [0, 1, 0, 0, 0,33, 0],
#         [0, 2,25, 4,34, 0,36],
#         [49,16, 0,23, 0, 0, 0],
#         [0,19, 0, 0,12, 7, 0],
#         [0, 0, 0,14, 0, 0, 0]
#         ]

  # Problems from the book:
  # Gyora Bededek: "Hidato: 2000 Pure Logic Puzzles"

  # Problem 1 (Practice)
  r = 5
  c = r
  puzzle = [
     [ 0, 0,20, 0, 0],
     [ 0, 0, 0,16,18],
     [22, 0,15, 0, 0],
     [23, 0, 1,14,11],
     [ 0,25, 0, 0,12],
     ]


#     # problem 2 (Practice)
  # r = 5
  # c = r
  # puzzle = [
  #     [0, 0, 0, 0, 14],
  #     [0, 18, 12, 0, 0],
  #     [0, 0, 17, 4, 5],
  #     [0, 0, 7, 0, 0],
  #     [9, 8, 25, 1, 0],
  # ]

  # problem 3 (Beginner)
#     r = 6
#     c = r
#     puzzle =  [
#         [ 0, 26,0, 0, 0,18],
#         [ 0, 0,27, 0, 0,19],
#         [31,23, 0, 0,14, 0],
#         [ 0,33, 8, 0,15, 1],
#         [ 0, 0, 0, 5, 0, 0],
#         [35,36, 0,10, 0, 0]
#         ];

  # Problem 15 (Intermediate)
  # Note: This takes very long time to solve...
#     r = 8
#     c = r
#     puzzle = [
#          [64, 0, 0, 0, 0, 0, 0, 0],
#          [ 1,63, 0,59,15,57,53, 0],
#          [ 0, 4, 0,14, 0, 0, 0, 0],
#          [ 3, 0,11, 0,20,19, 0,50],
#          [ 0, 0, 0, 0,22, 0,48,40],
#          [ 9, 0, 0,32,23, 0, 0,41],
#          [27, 0, 0, 0,36, 0,46, 0],
#          [28,30, 0,35, 0, 0, 0, 0]
#          ]

  print_game(puzzle, r, c)

  #
  # declare variables
  #
  x_flat = makeIntArray(sol,"x_flat",r*c, 1,r*c)
  x = {}
  for i in range(r):
    for j in range(c):
      x[(i, j)] = makeIntVar(sol, "x(%i,%i)" % (i, j), 1, r * c)
      sol.add(x_flat[i*r +j] == x[(i,j)])
  # x_flat = [x[(i, j)] for i in range(r) for j in range(c)]

  #
  # constraints
  #
  sol.add(Distinct(x_flat))

  #
  # Fill in the clues
  #
  for i in range(r):
    for j in range(c):
      if puzzle[i][j] > 0:
        sol.add(x[(i, j)] == puzzle[i][j])

  # From the numbers k = 1 to r*c-1, find this position,
  # and then the position of k+1
  cc = 0
  for k in range(1, r * c):
    i = makeIntVar(sol,"i_tmp_%i_%i" % (k,cc), 0, r)
    j = makeIntVar(sol,"j_tmp_%i_%i" % (k,cc), 0, c)
    a = makeIntVar(sol,"a_tmp_%i_%i" % (k,cc), -1, 1)
    b = makeIntVar(sol,"b_tmp_%i_%i" % (k,cc), -1, 1)
    cc += 1

    # 1) First: fix "this" k
    # sol.add(k == x[(i,j)])
    sol.add(k == x_flat[i * c + j])
   
    # 2) and then find the position of the next value (k+1)
    # solver.add(k + 1 == x[(i+a,j+b)])
    sol.add(k + 1 == x_flat[(i + a) * c + (j + b)])

    sol.add(i + a >= 0)
    sol.add(j + b >= 0)
    sol.add(i + a < r)
    sol.add(j + b < c)

    sol.add(Or(a != 0, b != 0))

  #
  # solution and search
  #
  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    xx_flat =  [mod.eval(x_flat[i*c+j]) for i in range(r) for j in range(c)]
    print("\nSolution:", num_solutions)
    print_board(mod, x, r, c)
    print()
    sol.add(Or([xx_flat[i*c+j] != x_flat[i*c+j] for i in range(r) for j in range(c) ]))

  print("num_solutions:", num_solutions)


def print_board(mod, x, rows, cols):
  for i in range(rows):
    for j in range(cols):
      print("% 2s" % mod.eval(x[i,j]), end=' ')
    print("")


def print_game(game, rows, cols):
  for i in range(rows):
    for j in range(cols):
      print("% 2s" % game[i][j], end=' ')
    print("")


if __name__ == "__main__":
  main()
