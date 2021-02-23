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

  Post office problem in OR-tools CP-SAT Solver.

  Problem statement:
  http://www-128.ibm.com/developerworks/linux/library/l-glpk2/

  From Winston 'Operations Research: Applications and Algorithms':
  '''
  A post office requires a different number of full-time employees working
  on different days of the week [summarized below]. Union rules state that
  each full-time employee must work for 5 consecutive days and then receive
  two days off. For example, an employee who works on Monday to Friday
  must be off on Saturday and Sunday. The post office wants to meet its
  daily requirements using only full-time employees. Minimize the number
  of employees that must be hired.

  To summarize the important information about the problem:

    * Every full-time worker works for 5 consecutive days and takes 2 days off
    * Day 1 (Monday): 17 workers needed
    * Day 2 : 13 workers needed
    * Day 3 : 15 workers needed
    * Day 4 : 19 workers needed
    * Day 5 : 14 workers needed
    * Day 6 : 16 workers needed
    * Day 7 (Sunday) : 11 workers needed

  The post office needs to minimize the number of employees it needs
  to hire to meet its demand.
  '''
  
  This is a port of my old CP model post_office_problem2.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import scalar_product

def main():

  model = cp.CpModel()

  #
  # data
  #

  # days 0..6, monday 0
  n = 7
  days = list(range(n))
  need = [17, 13, 15, 19, 14, 16, 11]

  # Total cost for the 5 day schedule.
  # Base cost per day is 100.
  # Working saturday is 100 extra
  # Working sunday is 200 extra.
  cost = [500, 600, 800, 800, 800, 800, 700]

  #
  # variables
  #

  # No. of workers starting at day i
  x = [model.NewIntVar(0, 100, 'x[%i]' % i) for i in days]

  total_cost = model.NewIntVar(0, 20000, 'total_cost')
  num_workers = model.NewIntVar(0, 100, 'num_workers')

  #
  # constraints
  #
  scalar_product(model, x, cost, total_cost)
  model.Add(num_workers == sum(x))

  for i in days:
    model.Add(sum(
        [x[j] for j in days if j != (i + 5) % n and j != (i + 6) % n]) >= need[i])

  # objective
  model.Minimize(total_cost)

  #
  # search and result
  #
  solver = cp.CpSolver()
  status = solver.Solve(model)
  
  if status == cp.OPTIMAL:
    print('num_workers:', solver.Value(num_workers))
    print('total_cost:', solver.Value(total_cost))
    print('x:', [solver.Value(x[i]) for i in days])


  print()
  print('NumConflicts:', solver.NumConflicts())
  print('NumBranches:', solver.NumBranches())
  print('WallTime:', solver.WallTime())


if __name__ == '__main__':
  main()
