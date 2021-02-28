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

  Nurse rostering in OR-tools CP-SAT Solver.

  This is a simple nurse rostering model using a DFA and
  AddAutomaton.

  The DFA is from MiniZinc Tutorial, Nurse Rostering example:
  - one day off every 4 days
  - no 3 nights in a row.
  See https://www.minizinc.org/tutorial/minizinc-tute.pdf 
  section 4.1.4 "Regular" for details.

  Compare with nurse_rostering_sat.py which use my slower decomposition
  the MiniZinc style regular constraint.


  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/

"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import regular_table, regular_element
from collections import defaultdict


class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, num_nurses, num_days, days, shifts, x, nurse_stat, day_stat):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__num_nurses = num_nurses
        self.__num_days = num_days
        self.__days = days
        self.__shifts = shifts
        self.__x = x
        self.__nurse_stat = nurse_stat
        self.__day_stat = day_stat
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1

        for i in range(self.__num_nurses):
          print('Nurse%i: ' % i, end=' ')
          this_day_stat = defaultdict(int)
          for j in range(self.__num_days):
            d = self.__days[self.Value(self.__x[i, j]) - 1]
            this_day_stat[d] += 1
            print(d, end=' ')
          print(
              ' day_stat:', [(d, this_day_stat[d]) for d in this_day_stat], end=' ')
          print('total:', self.Value(self.__nurse_stat[i]), 'workdays')
        print()

        print('Statistics per day:')
        for j in range(self.__num_days):
          print('Day%2i: ' % j, end=' ')
          for t in self.__shifts:
            print(self.Value(self.__day_stat[j, t]), end=' ')
          print()
        print()

        # We just show 2 solutions
        if self.__solution_count >= 2:
          print("Two solutions should be enough...")
          self.StopSearch()



    def SolutionCount(self):
        return self.__solution_count


def main():

  model = cp.CpModel()

  #
  # data
  #

  # Note: If you change num_nurses or num_days,
  #       please also change the constraints
  #       on nurse_stat and/or day_stat below.
  num_nurses = 7
  num_days = 14

  # The different shifts
  day_shift   = 1
  night_shift = 2
  off_shift   = 3
  shifts = [day_shift, night_shift, off_shift]

  # The old MiniZinc style regular transitions
  # (cf MiniZinc Tutorial cited above)
  # transitions = [
  #     # d,n,o
  #     [2, 3, 1],  # state 1
  #     [4, 4, 1],  # state 2
  #     [4, 5, 1],  # state 3
  #     [6, 6, 1],  # state 4
  #     [6, 0, 1],  # state 5
  #     [0, 0, 1]   # state 6
  # ]

  # Transitions for AddAutomaton
  transitions = [
    (1,off_shift,1),
    (1,day_shift,2),
    (1,night_shift,3),
    (2,off_shift,1),
    (2,day_shift,4),
    (2,night_shift,4),
    (3,day_shift,4),
    (3,off_shift,1),
    (4,day_shift,6),
    (4,off_shift,1),
    (4,night_shift,6),
    (5,day_shift,6),
    (5,off_shift,1),
    (6,off_shift,1),
  ]

  initial_state = 1
  accepting_states = [1, 2, 3, 4, 5, 6]

  days = ['d', 'n', 'o']  # for presentation

  #
  # declare variables
  #
  x = {}
  for i in range(num_nurses):
    for j in range(num_days):
      x[i, j] = model.NewIntVar(day_shift, off_shift, 'x[%i,%i]' % (i, j))

  # summary of the nurses
  nurse_stat = [
      model.NewIntVar(0, num_days, 'nurse_stat[%i]' % i)
      for i in range(num_nurses)
  ]

  # summary of the shifts per day
  day_stat = {}
  for i in range(num_days):
    for j in shifts:
      day_stat[i, j] = model.NewIntVar(0, num_nurses, 'day_stat[%i,%i]' % (i, j))

  #
  # constraints
  #
  for i in range(num_nurses):
    y = [x[i, j] for j in range(num_days)]
    model.AddAutomaton(y, initial_state, accepting_states, transitions)

  #
  # Statistics and constraints for each nurse
  #
  for i in range(num_nurses):
    # number of worked days (day or night shift)
    b_ds = [model.NewBoolVar("") for j in range(num_days)] # day shift
    b_ns = [model.NewBoolVar("") for j in range(num_days)] # night shift
    for j in range(num_days):
        model.Add(x[i,j]==day_shift).OnlyEnforceIf(b_ds[j]) 
        model.Add(x[i,j]!=day_shift).OnlyEnforceIf(b_ds[j].Not()) 
        
        model.Add(x[i,j]==night_shift).OnlyEnforceIf(b_ns[j]) 
        model.Add(x[i,j]!=night_shift).OnlyEnforceIf(b_ns[j].Not()) 
    model.Add(nurse_stat[i] == sum(b_ds + b_ns))

    # Each nurse must work between 7 and 10
    # days during this period
    model.Add(nurse_stat[i] >= 7)
    model.Add(nurse_stat[i] <= 10)

  #
  # Statistics and constraints for each day
  #
  for j in range(num_days):
    for t in shifts:
      b = [model.NewBoolVar("") for i in range(num_nurses)]
      for i in range(num_nurses):
        model.Add(x[i,j] == t).OnlyEnforceIf(b[i])
        model.Add(x[i,j] != t).OnlyEnforceIf(b[i].Not())
      model.Add(day_stat[j, t] == sum(b))

    #
    # Some constraints for this day:
    #
    # Note: We have a strict requirements of
    #       the number of shifts.
    #       Using atleast constraints is much harder
    #       in this model.
    #
    if j % 7 == 5 or j % 7 == 6:
      # special constraints for the weekends
      model.Add(day_stat[j, day_shift] == 2)
      model.Add(day_stat[j, night_shift] == 1)
      model.Add(day_stat[j, off_shift] == 4)
    else:
      # workdays:

      # - exactly 3 on day shift
      model.Add(day_stat[j, day_shift] == 3)
      # - exactly 2 on night
      model.Add(day_stat[j, night_shift] == 2)
      # - exactly 2 off duty
      model.Add(day_stat[j, off_shift] == 2)

  #
  # solution and search
  #
  solver = cp.CpSolver() 
  solution_printer = SolutionPrinter(num_nurses, num_days, days, shifts, x, nurse_stat, day_stat)
  status = solver.SearchForAllSolutions(model, solution_printer)

  if not status in [cp.OPTIMAL, cp.FEASIBLE]:
    print("No solution!")


  print()
  print('NumSolutions:', solution_printer.SolutionCount())
  print('NumConflicts:', solver.NumConflicts())
  print('NumBranches:', solver.NumBranches())
  print('WallTime:', solver.WallTime())


if __name__ == '__main__':
  main()
