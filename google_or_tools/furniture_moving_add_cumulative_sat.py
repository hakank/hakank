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

  Moving furnitures (scheduling) problem in OR-tools CP-SAT Solver.

  Marriott & Stukey: 'Programming with constraints', page  112f

  The model use the built-in constraint AddCumulative.

  Compare with furniture_moving_sat.py which use a decomposition
  of cumulative.

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import my_cumulative


def main():

  model = cp.CpModel()

  #
  # data
  #
  n = 4
  duration = [30, 10, 15, 15]
  demand = [3, 1, 3, 2]
  upper_limit = 160

  #
  # declare variables
  #
  start_times = [
      model.NewIntVar(0, upper_limit, "start_times[%i]" % i) for i in range(n)
  ]
  end_times = [
      model.NewIntVar(0, upper_limit * 2, "end_times[%i]" % i) for i in range(n)
  ]
  end_time = model.NewIntVar(0, upper_limit * 2, "end_time")

  intervals = [model.NewIntervalVar(start_times[i],duration[i],end_times[i],f"interval[{i}]")  for i in range(n)]


  # number of needed resources, to be minimized
  num_resources = model.NewIntVar(1, 10, "num_resources")

  #
  # constraints
  #
  model.AddMaxEquality(end_time,end_times)
  
  model.AddCumulative(intervals, demand, num_resources)

  #
  # Some extra constraints to play with
  #

  # all tasks must end within an hour
  model.Add(end_time <= 60)

  # All tasks should start at time 0
  # for i in range(n):
  #    model.Add(start_times[i] == 0)

  # limitation of the number of people
  # model.Add(num_resources <= 3)

  #
  # objective
  #
  # model.Minimize(end_time)
  model.Minimize(num_resources)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  status = solver.Solve(model)
  #
  # result
  #
  if status == cp.OPTIMAL:
    print("num_resources:", solver.Value(num_resources))
    print("start_times  :", [solver.Value(start_times[i]) for i in range(n)])
    print("duration     :", [duration[i] for i in range(n)])
    print("end_times    :", [solver.Value(end_times[i]) for i in range(n)])
    print("end_time     :", solver.Value(end_time))
    print()

  print()
  # print("num_solutions:", num_solutions)
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


if __name__ == "__main__":
  main()
