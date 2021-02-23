# Copyright 2021 Hakan Kjellerstrand hakank@gmail.com
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
"""
  Nontransitive dice in OR-tools CP-SAT Solver.

  From
  http://en.wikipedia.org/wiki/Nontransitive_dice
  '''
  A set of nontransitive dice is a set of dice for which the relation
  'is more likely to roll a higher number' is not transitive. See also
  intransitivity.

  This situation is similar to that in the game Rock, Paper, Scissors,
  in which each element has an advantage over one choice and a
  disadvantage to the other.
  '''

  I start with the 3 dice version
  '''
     * die A has sides {2,2,4,4,9,9},
     * die B has sides {1,1,6,6,8,8}, and
     * die C has sides {3,3,5,5,7,7}.
  '''
    3 dice:
    Maximum winning: 27
    comp: [19, 27, 19]
    dice:
    [[0, 0, 3, 6, 6, 6],
    [2, 5, 5, 5, 5, 5],
    [1, 1, 4, 4, 4, 7]]
    max_win: 27

  Max winnings where they are the same: 21
    comp: [21, 21, 21]
    dice:
    [[0, 0, 3, 3, 3, 6],
    [2, 2, 2, 2, 2, 5],
    [1, 1, 1, 4, 4, 4]]
    max_win: 21

  This is a port of my old CP model nontransitive_dice.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import global_cardinality

class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, dice, m, n, gap, gap_sum, max_val, max_win, comp, counts):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__dice = dice
        self.__m = m
        self.__n = n
        self.__gap = gap
        self.__gap_sum = gap_sum
        self.__max_val = max_val
        self.__max_win = max_win
        self.__comp = comp
        self.__counts = counts
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        print(f"Solution #{self.__solution_count}")
        print("gap_sum:", self.Value(self.__gap_sum))
        print("gap:", [self.Value(self.__gap[i]) for i in range(self.__m)])
        print("max_val:", self.Value(self.__max_val))
        print("max_win:", self.Value(self.__max_win))
        print("dice:")
        for i in range(self.__m):
          for j in range(self.__n):
            print(self.Value(self.__dice[(i, j)]), end=" ")
          print()
        print("comp:")
        for i in range(self.__m):
          for j in range(2):
            print(self.Value(self.__comp[(i, j)]), end=" ")
          print()
        print("counts:", [self.Value(self.__counts[i]) for i in range(n * 2 + 1)])
        print()

    def SolutionCount(self):
        return self.__solution_count



def main(m=3, n=6, minimize_val=0):

  model = cp.CpModel()

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
      dice[(i, j)] = model.NewIntVar(1, n * 2, "dice(%i,%i)" % (i, j))
  dice_flat = [dice[(i, j)] for i in range(m) for j in range(n)]

  comp = {}
  for i in range(m):
    for j in range(2):
      comp[(i, j)] = model.NewIntVar(0, n * n, "comp(%i,%i)" % (i, j))
  comp_flat = [comp[(i, j)] for i in range(m) for j in range(2)]

  # The following variables are for summaries or objectives
  gap = [model.NewIntVar(0, n * n, "gap(%i)" % i) for i in range(m)]
  gap_sum = model.NewIntVar(0, m * n * n, "gap_sum")

  max_val = model.NewIntVar(0, n * 2, "max_val")
  max_win = model.NewIntVar(0, n * n, "max_win")

  # number of occurrences of each value of the dice
  counts = [model.NewIntVar(0, n * m, "counts(%i)" % i) for i in range(n * 2 + 1)]

  #
  # constraints
  #

  # number of occurrences for each number
  # model.Add(solver.Distribute(dice_flat, list(range(n * 2 + 1)), counts))
  global_cardinality(model, dice_flat, list(range(n * 2 + 1)), counts)

  model.AddMaxEquality(max_win,comp_flat)
  model.AddMaxEquality(max_val,dice_flat)

  # order of the number of each die, lowest first
  [
      model.Add(dice[(i, j)] <= dice[(i, j + 1)])
      for i in range(m)
      for j in range(n - 1)
  ]

  # nontransitivity
  [comp[i, 0] > comp[i, 1] for i in range(m)],

  # probability gap
  [model.Add(gap[i] == comp[i, 0] - comp[i, 1]) for i in range(m)]
  [model.Add(gap[i] > 0) for i in range(m)]
  model.Add(gap_sum == sum(gap))

  # and now we roll...
  #  Number of wins for [A vs B, B vs A]
  for d in range(m):
    d1 = d % m     
    d2 = (d+1) % m 
    # d1 > d2
    b1 = []
    for r1 in range(n):
      for r2 in range(n):
        b = model.NewBoolVar("")
        model.Add(dice[d1, r1] > dice[d2, r2]).OnlyEnforceIf(b) 
        model.Add(dice[d1, r1] <= dice[d2, r2]).OnlyEnforceIf(b.Not()) 
        b1.append(b)
    model.Add(comp[d % m, 0] == sum(b1))
    # d2 > d1
    b2 = []
    for r1 in range(n):
      for r2 in range(n):
        b = model.NewBoolVar("")
        model.Add(dice[d2, r1] > dice[d1, r2]).OnlyEnforceIf(b)
        model.Add(dice[d2, r1] <= dice[d1, r2]).OnlyEnforceIf(b.Not())
        b2.append(b)
    model.Add(comp[d % m, 1] == sum(b2))

  # objective
  if minimize_val != 0:
    print("Minimizing max_val")
    model.Minimize(max_val)
    # other experiments
    # model.Maximize(max_win)
    # model.Maximize(gap_sum)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  # status = solver.Solve(model)
  solution_printer = SolutionPrinter(dice, m, n, gap, gap_sum, max_val, max_win, comp, counts)
  if minimize_val != 0:
    status = solver.Solve(model)
  else:
    status = solver.SearchForAllSolutions(model, solution_printer)

  if status == cp.OPTIMAL:
    print("gap_sum:", solver.Value(gap_sum))
    print("gap:", [solver.Value(gap[i]) for i in range(m)])
    print("max_val:", solver.Value(max_val))
    print("max_win:", solver.Value(max_win))
    print("dice:")
    for i in range(m):
      for j in range(n):
        print(solver.Value(dice[(i, j)]), end=" ")
      print()
    print("comp:")
    for i in range(m):
      for j in range(2):
        print(solver.Value(comp[(i, j)]), end=" ")
      print()
    print("counts:", [solver.Value(counts[i]) for i in range(n * 2 + 1)])
    print()


  print()
  print("NumSolutions:", solution_printer.SolutionCount())
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


m = 3  # number of dice
n = 6  # number of sides of each die
minimize_val = 0  # Minimizing max value (0: no, 1: yes)
if __name__ == "__main__":
  if len(sys.argv) > 1:
    m = int(sys.argv[1])
  if len(sys.argv) > 2:
    n = int(sys.argv[2])
  if len(sys.argv) > 3:
    minimize_val = int(sys.argv[3])

  main(m, n, minimize_val)
