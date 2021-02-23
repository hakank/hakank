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

  Bus scheduling in Google CP Solver.


  Problem from Taha "Introduction to Operations Research", page 58.

  It's a slightly more general model than Taha's.

  This is a port of my old OR-tools CP model bus_schedule.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other Google CP Solver models:
  http://www.hakank.org/google_or_tools/

"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import array_values


class SolutionPrinter(cp.CpSolverSolutionCallback):
    def __init__(self, x, num_buses):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__x = x
        self.__num_buses = num_buses
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        print("x:", array_values(self,self.__x), end= " ")
        print("num_buses:", self.Value(self.__num_buses))

    def SolutionCount(self):
        return self.__solution_count


def main(num_buses_check=0):

  model = cp.CpModel()

  # data
  time_slots = 6
  demands = [8, 10, 7, 12, 4, 4]
  max_num = sum(demands)

  # declare variables
  x = [model.NewIntVar(0, max_num, "x%i" % i) for i in range(time_slots)]
  num_buses = model.NewIntVar(0, max_num, "num_buses")

  #
  # constraints
  #
  model.Add(num_buses == sum(x))

  # Meet the demands for this and the next time slot
  for i in range(time_slots - 1):
    model.Add(x[i] + x[i + 1] >= demands[i])

  # The demand "around the clock"
  model.Add(x[time_slots - 1] + x[0] == demands[time_slots - 1])

  if num_buses_check > 0:
    model.Add(num_buses == num_buses_check)
  else: 
    # objective
    model.Minimize(num_buses)

  #
  # solution and search
  #
  solver = cp.CpSolver()

  solution_printer = SolutionPrinter(x,num_buses)
  if num_buses_check == 0:
    status = solver.Solve(model)
  else:
    status = solver.SearchForAllSolutions(model, solution_printer)

  if status == cp.OPTIMAL and num_buses_check == 0:
    print("x:", [solver.Value(x[i]) for i in range(len(x))], end=" ")
    print(" num_buses:", solver.Value(num_buses))

  print()
  print("NumSolutions:", solution_printer.SolutionCount())
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())
  print()
  if num_buses_check == 0:
    return solver.Value(num_buses)


if __name__ == "__main__":
  print("Check for minimun number of buses")
  num_buses_check = main()
  print("Optimal value:", num_buses_check, "buses")
  print("All solutions:")
  main(num_buses_check)
