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
  
  Number lock problem (Crack the code) in OR-tools CP-SAT Solver.

  From Presh Talwalkar (MindYourDecisions) 
  '''
  Puzzles like this have been shared with the dubious claim that "only a
  genius can solve" them. But they are still fun problems so let's work one
  out.

  A number lock requires a 3 digit code. Based on these hints, can you crack
  the code?

    682 - one number is correct and in the correct position
    645 - one number is correct but in the wrong position
    206 - two numbers are correct but in the wrong positions
    738 - nothing is correct
    780 - one number is correct but in the wrong position

  Video:  https://youtu.be/-etLb-8sHBc
  '''

  Today Moshe Vardi published a related problem
  (https://twitter.com/vardi/status/1164204994624741376 )
  where all hints, except for the second, where identical:

    682 - one number is correct and in the correct position
    614 - one number is correct but in the wrong position    <-- This is different.
    206 - two numbers are correct but in the wrong positions
    738 - nothing is correct
    780 - one number is correct but in the wrong position

  Also, see https://dmcommunity.org/challenge/challenge-sep-2019/

  This model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OR-tools page: http://www.hakank.org/or_tools/

"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import ListPrinter


def check(model, a, b, pos, val):
  """
  `a` and `b`: the two arrays to check (here `b` 
  corresponds to the array `x` in the model).
        
  `pos`: number of correct values and positions
  `val`: numbver correct values 
         (regardless if there are correct position or not)
  """
  n = len(a)

  # number of entries in correct position (and correct values)
  # sum([a[j] == b[j] : j in 1..n]) == pos
  pos_b = [model.NewBoolVar(f"pos_b[{i}]") for i in range(n)]
  for i in range(n):
    model.Add(a[i] == b[i]).OnlyEnforceIf(pos_b[i])
    model.Add(a[i] != b[i]).OnlyEnforceIf(pos_b[i].Not())
  model.Add(pos == sum(pos_b))

  # number of entries which has correct values 
  # (regardless if there are in correct position or not)
  # sum([a[j] == b[k] : j in 1..n, k in 1..n ]) == val
  sum_bs = [] 
  for i in range(n):
    for j in range(n):
      bb = model.NewBoolVar(f"b[{i,j}]")
      model.Add(a[i] == b[j]).OnlyEnforceIf(bb)
      model.Add(a[i] != b[j]).OnlyEnforceIf(bb.Not())
      sum_bs.append(bb)
  model.Add(val == sum(sum_bs))


def main(problem):

  model = cp.CpModel()

  # data
  n = len(problem[0][0]) # number of digits
  
  # decision variabels
  x = [model.NewIntVar(0,9,f"x[{i}]") for i in range(n)]

  # constraints
  for digits, num_correct_pos, num_correct_number in problem:
    check(model, digits, x, num_correct_pos, num_correct_number)

  # search and solution
  solver  = cp.CpSolver()
  solution_printer = ListPrinter(x)
  status = solver.SearchForAllSolutions(model, solution_printer)
  if not status in [cp.OPTIMAL, cp.FEASIBLE]:
    print("No solution!")

  print()
  print("NumSolutions:", solution_printer.SolutionCount())  
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())
  print()


# From Presh Talwalkar (MindYourDecisions) 
# See above.
problem1 = [
    [[6,8,2],1,1], # - one number is correct and in the correct position
    [[6,4,5],0,1], # - one number is correct but in the wrong position    
    [[2,0,6],0,2], # - two numbers are correct but in the wrong positions
    [[7,3,8],0,0], # - nothing is correct
    [[7,8,0],0,1]  # - one number is correct but in the wrong position
  ]


# From Moshe Vardi
# See above
problem2 = [
    [[6,8,2],1,1], # - one number is correct and in the correct position
    [[6,1,4],0,1], # - one number is correct but in the wrong position    
    [[2,0,6],0,2], # - two numbers are correct but in the wrong positions
    [[7,3,8],0,0], # - nothing is correct
    [[7,8,0],0,1]  # - one number is correct but in the wrong position
  ]


if __name__ == '__main__':
  print("Problem1:")
  main(problem1)
  
  print("Problem2:")
  main(problem2)
