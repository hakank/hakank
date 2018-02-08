#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Nontransitive dice in Z3
#
#  From
#  http://en.wikipedia.org/wiki/Nontransitive_dice
#  '''
#  A set of nontransitive dice is a set of dice for which the relation
#  'is more likely to roll a higher number' is not transitive. See also
#  intransitivity.
#
#  This situation is similar to that in the game Rock, Paper, Scissors,
#  in which each element has an advantage over one choice and a
#  disadvantage to the other.
#  '''
#
#  I start with the 3 dice version
#  '''
#     * die A has sides {2,2,4,4,9,9},
#     * die B has sides {1,1,6,6,8,8}, and
#     * die C has sides {3,3,5,5,7,7}.
#  '''
#
#  3 dice:
#  Maximum winning: 27
#  comp: [19, 27, 19]
#  dice:
#  [[0, 0, 3, 6, 6, 6],
#  [2, 5, 5, 5, 5, 5],
#  [1, 1, 4, 4, 4, 7]]
#  max_win: 27
#
#  Number of solutions:  1
#
# Max winnings where they are the same: 21
#   comp: [21, 21, 21]
#   dice:
#   [[0, 0, 3, 3, 3, 6],
#   [2, 2, 2, 2, 2, 5],
#   [1, 1, 1, 4, 4, 4]]
#   max_win: 21

# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
from z3_utils_hakank import *


def main(m=3, n=6, minimize_val=0):

  sol = Solver()

  #
  # data
  #
  print("number of dice:", m)
  print("number of sides:", n)

  #
  # declare variables
  #

  dice = {}
  for i in range(m):
    for j in range(n):
      dice[(i, j)] = makeIntVar(sol, "dice(%i,%i)" % (i, j),1, n * 2)
  dice_flat = [dice[(i, j)] for i in range(m) for j in range(n)]

  comp = {}
  for i in range(m):
    for j in range(2):
      comp[(i, j)] = makeIntVar(sol, "comp(%i,%i)" % (i, j), 0, n * n)
  comp_flat = [comp[(i, j)] for i in range(m) for j in range(2)]

  # The following variables are for summaries or objectives
  gap = [makeIntVar(sol, "gap(%i)" % i, 0, n * n) for i in range(m)]
  gap_sum = makeIntVar(sol, "gap_sum", 0, m * n * n)

  max_val = makeIntVar(sol, "max_val", 0, n * 2)
  max_win = makeIntVar(sol, "max_win", 0, n * n)

  # number of occurrences of each value of the dice
  counts = [makeIntVar(sol, "counts(%i)" % i, 0, n * m) for i in range(n * 2 + 1)]

  #
  # constraints
  #

  # number of occurrences for each number
  global_cardinality_count(sol,list(range(n * 2 + 1)), dice_flat, counts)

  # sol.add(max_win == max(comp_flat))
  maximum(sol,max_win, comp_flat)
  # sol.add(max_val == max(dice_flat))
  maximum(sol,max_val, dice_flat)

  # order of the number of each die, lowest first
  [sol.add(dice[(i, j)] <= dice[(i, j + 1)])
   for i in range(m) for j in range(n - 1)]

  # nontransitivity
  [comp[i, 0] > comp[i, 1] for i in range(m)],

  # probability gap
  [sol.add(gap[i] == comp[i, 0] - comp[i, 1]) for i in range(m)]
  [sol.add(gap[i] > 0) for i in range(m)]
  sol.add(gap_sum == Sum(gap))

  # and now we roll...
  #  Number of wins for [A vs B, B vs A]
  for d in range(m):
    sol.add(comp[d % m, 0] ==
            Sum([If(dice[d % m, r1] > dice[(d + 1) % m, r2],1,0)
                 for r1 in range(n) for r2 in range(n)]))    
    sol.add(comp[d % m, 1] ==
            Sum([If(dice[(d + 1) % m, r1] > dice[d % m, r2],1,0)
                 for r1 in range(n) for r2 in range(n)]))

  # objective (see below)
  # if minimize_val != 0:
  #   print("Minimizing max_val")
  #   sol.minimize(max_val)
  #   # other experiments
  #   # sol.maximize(max_win)
  #   # sol.maximize(gap_sum)

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print("gap_sum:", mod.eval(gap_sum))
    print("gap:", [mod.eval(gap[i]) for i in range(m)])
    print("max_val:", mod.eval(max_val))
    print("max_win:", mod.eval(max_win))
    print("dice:")
    for i in range(m):
      for j in range(n):
        print(mod.eval(dice[(i, j)]), end=' ')
      print()
    print("comp:")
    for i in range(m):
      for j in range(2):
        print(mod.eval(comp[(i, j)]), end=' ')
      print()
    print("counts:", [mod.eval(counts[i]) for i in range(n * 2 + 1)])
    print()
    if minimize_val != 0:
        getLessSolution(sol,mod,max_val)
        # getGreaterSolution(sol,mod,max_win)
        # getGreaterSolution(sol,mod,gap_sum)    
    else:
        getDifferentSolution(sol,mod,dice_flat)

  print()
  print("num_solutions:", num_solutions)


m = 3             # number of dice
n = 6             # number of sides of each die
minimize_val = 0  # Minimizing max value (0: no, 1: yes)
if __name__ == "__main__":
  if len(sys.argv) > 1:
    m = int(sys.argv[1])
  if len(sys.argv) > 2:
    n = int(sys.argv[2])
  if len(sys.argv) > 3:
    minimize_val = int(sys.argv[3])

  main(m, n, minimize_val)
