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
# This is a version of hidato.py that uses Function()
# Also, see hidato_table.py

# Here is a comparison of solve times for the different
# Hidato programs. '(*)' marks the best time.
#
# Time to first solution
# ======================
# Problem    hidato     hidato_table   hidato_function
# ----------------------------------------------------
# 1           0.041s    0.052s         0.023s(*)
# 2           3.228s    2.107s         0.325s(*)
# 3           0.310s    0.479s         0.179s(*)
# 4           0.265s    0.480s         0.071s(*)
# 5           1.676s    1.068s         0.156s(*)
# 6          11.532s    3.684s         0.614s(*)
# 7          86.420s    9.886s         5.056s(*)
# 8         356.038s   20.580s         3.864s(*)
#
# Time to prove unicity
# =====================
# Problem    hidato     hidato_table   hidato_function
# -----------------------------------------------------
# 1          0.042s     0.049s         0.029s(*)
# 2          3.936s     2.098s         0.586s(*)
# 3          0.340s     0.497s         0.212s(*)
# 4          0.263s     0.489s         0.151s(*)
# 5          1.856s     1.096s         0.236s(*)
# 6         13.294s     3.738s         1.245s(*)
# 7         92.718s    10.018s(*)     14.849s
# 8        414.065s    20.638s         7.405s(*)
#
#
# With one interesting exception (problem 7), this Hidato program with Function
# is the fastest.
#
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from __future__ import print_function
import time
from hidato_instances import instances
from z3_utils_hakank import *

def hidato_function(puzzle,num_sols=0):

  sol = SolverFor("AUFLIA")

  r = len(puzzle)
  c = len(puzzle[0])
  print_game(puzzle, r, c)

  #
  # declare variables
  #
  # x(int,int) -> int  
  x = Function("x",IntSort(), IntSort(), IntSort()) 
  x_flat = [x(i,j) for i in range(r) for j in range(c)]

  # constraints
  sol.add(Distinct(x_flat))

  #
  # Fill in the clues
  #
  for i in range(r):
    for j in range(c):
      if puzzle[i][j] > 0:
        sol.add(x(i, j) == puzzle[i][j])

  # From the numbers k = 1 to r*c-1, find this position,
  # and then the position of k+1
  cc = 0
  for k in range(1, r * c):
    i = makeIntVar(sol,"i_tmp_%i_%i" % (k,cc), 0, r-1)
    j = makeIntVar(sol,"j_tmp_%i_%i" % (k,cc), 0, c-1)
    a = makeIntVar(sol,"a_tmp_%i_%i" % (k,cc), -1, 1)
    b = makeIntVar(sol,"b_tmp_%i_%i" % (k,cc), -1, 1)
    cc += 1

    # 1) First: fix "this" k
    sol.add(k == x(i,j))
   
    # 2) and then find the position of the next value (k+1)
    sol.add(k + 1 == x(i+a,j+b))

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
    print()
    print_board(mod, x, r, c)
    print()
    if num_sols > 0 and num_solutions >= num_sols:
      break
    sol.add(Or([x(i,j) != mod.eval(x(i,j)) for i in range(r) for j in range(c) ]))

  print("num_solutions:", num_solutions)


def print_board(mod, x, rows, cols):
  for i in range(rows):
    for j in range(cols):
      print("% 4s" % mod.eval(x(i,j)), end=' ')
    print("")


def print_game(game, rows, cols):
  for i in range(rows):
    for j in range(cols):
      print("% 4s" % game[i][j], end=' ')
    print("")


def test_all(num_sols=0):
  times = {}
  for puzzle in instances:
    print()
    print(f"----- Solving problem {puzzle} -----")
    print()
    t0 = time.time()
    hidato_function(instances[puzzle],num_sols)
    t1 = time.time()
    print("Time:", t1-t0)
    times[puzzle] = t1-t0
    print()

  print("Times:")
  for puzzle in times:
    print(puzzle, ":", times[puzzle])

print("\nTime to first solution:")
num_sols = 1
test_all(num_sols)

print("Time to prove unicity:")
num_sols = 0
test_all(num_sols)
