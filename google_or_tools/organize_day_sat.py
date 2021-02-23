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

  Organizing a day in OR-tools CP-SAT Solver.

  Simple scheduling problem.

  Problem formulation from ECLiPSe:
  Slides on (Finite Domain) Constraint Logic Programming, page 38f
  http://eclipse-clp.org/reports/eclipse.ppt


  This is a port of my old CP model organize_day.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import no_overlap


class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, tasks, begins, ends):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__tasks = tasks
        self.__begins = begins 
        self.__ends = ends
        
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        print('begins:', [self.Value(self.__begins[i]) for i in self.__tasks])
        print('ends  :', [self.Value(self.__ends[i]) for i in self.__tasks])
        print()

    def SolutionCount(self):
        return self.__solution_count



def main():

  model = cp.CpModel()

  #
  # data
  #
  n = 4

  tasks = list(range(n))
  work, mail, shop, bank = tasks
  durations = [4, 1, 2, 1]

  # task [i,0] must be finished before task [i,1]
  before_tasks = [[bank, shop], [mail, work]]

  # the valid times of the day
  begin = 9
  end = 17

  #
  # declare variables
  #
  begins = [model.NewIntVar(begin, end, 'begins[%i]% % i') for i in tasks]
  ends = [model.NewIntVar(begin, end, 'ends[%i]% % i') for i in tasks]

  #
  # constraints
  #
  for i in tasks:
    model.Add(ends[i] == begins[i] + durations[i])

  for i in tasks:
    for j in tasks:
      if i < j:
        no_overlap(model, begins[i], durations[i], begins[j], durations[j])

  # specific constraints
  for (before, after) in before_tasks:
    model.Add(ends[before] <= begins[after])

  model.Add(begins[work] >= 11)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  solution_printer = SolutionPrinter(tasks, begins, ends)
  _status = solver.SearchForAllSolutions(model, solution_printer)

  
  # if status == cp.OPTIMAL:
  #   print('begins:', [solver.Value(begins[i]) for i in tasks])
  #   print('ends:', [solver.Value(ends[i]) for i in tasks])
  #   print()

  print('NumSolutions:', solution_printer.SolutionCount())
  print('NumConflicts:', solver.NumConflicts())
  print('NumBranches:', solver.NumBranches())
  print('WallTime:', solver.WallTime())


if __name__ == '__main__':
  main()
