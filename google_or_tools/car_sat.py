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

  Car sequencing in Google CP Solver.

  This model is based on the car sequencing model in
  Pascal Van Hentenryck
  'The OPL Optimization Programming Language', page 184ff.

  It is port of my of OR-tools CP model car.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import count_vars

class SolutionPrinter(cp.CpSolverSolutionCallback):
    def __init__(self, slot,Slots,Options,capacity,setup,num_sol):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__slot = slot
        self.__Slots = Slots
        self.__Options = Options
        self.__capacity = capacity
        self.__setup = setup
        self.__num_sol = num_sol
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        print(f"Solution #{self.__solution_count}")
        print("slot:%s" % ",".join([str(self.Value(self.__slot[i])) for i in self.__Slots]))
        print("setup:")
        for o in self.__Options:
          print("%i/%i:" % (self.__capacity[o][0], self.__capacity[o][1]), end=" ")
          for s in self.__Slots:
            print(self.Value(self.__setup[o, s]), end=" ")
          print()
        print()
        if self.__solution_count >= self.__num_sol:
          self.StopSearch()

    def SolutionCount(self):
        return self.__solution_count



def main(num_sol=3):

  model = cp.CpModel()

  # data
  nbCars = 6
  nbOptions = 5
  nbSlots = 10

  Cars = list(range(nbCars))
  Options = list(range(nbOptions))
  Slots = list(range(nbSlots))

  #    car 0   1  2  3  4  5
  demand = [1, 1, 2, 2, 2, 2]

  option = [
      # car 0  1  2  3  4  5
      [1, 0, 0, 0, 1, 1],  # option 1
      [0, 0, 1, 1, 0, 1],  # option 2
      [1, 0, 0, 0, 1, 0],  # option 3
      [1, 1, 0, 1, 0, 0],  # option 4
      [0, 0, 1, 0, 0, 0]  # option 5
  ]

  capacity = [(1, 2), (2, 3), (1, 3), (2, 5), (1, 5)]

  optionDemand = [
    sum([demand[j] * option[i][j] for j in Cars]) for i in Options
  ]

  #
  # variables
  #
  slot = [model.NewIntVar(0, nbCars - 1, "slot[%i]" % i) for i in Slots]
  setup = {}
  for i in Options:
    for j in Slots:
      setup[(i, j)] = model.NewIntVar(0, 1, "setup[%i,%i]" % (i, j))

  #
  # constraints
  #
  for c in Cars:
    count_vars(model,slot,c,demand[c])
    

  for o in Options:
    for s in range(0, nbSlots - capacity[o][1] + 1):
      b = [setup[o, j] for j in range(s, s + capacity[o][1] - 1)]
      model.Add(sum(b) <= capacity[o][0])

  for o in Options:
    for s in Slots:
      model.AddElement(slot[s], option[o], setup[(o, s)])

  for o in Options:
    for i in range(optionDemand[o]):
      s_range = list(range(0, nbSlots - (i + 1) * capacity[o][1]))
      ss = [setup[o, s] for s in s_range]
      cc = optionDemand[o] - (i + 1) * capacity[o][0]
      if len(ss) > 0 and cc >= 0:
        model.Add(sum(ss) >= cc)

  #
  # search and result
  #
  solver = cp.CpSolver()
  solution_printer = SolutionPrinter(slot,Slots,Options,capacity,setup,num_sol)
  status = solver.SearchForAllSolutions(model, solution_printer)
  print("status:", solver.StatusName(status))

  print()
  print("NumSolutions:", solution_printer.SolutionCount())
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


num_sol = 3
if __name__ == "__main__":
  if len(sys.argv) > 1:
    num_sol = int(sys.argv[1])
  main(num_sol)
