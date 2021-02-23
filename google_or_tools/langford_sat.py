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

  Langford's number problem in OR-tools CP-SAT Solver.

  This is a port of my old CP model langford.py

  Langford's number problem (CSP lib problem 24)
  http://www.csplib.org/prob/prob024/
  '''
  Arrange 2 sets of positive integers 1..k to a sequence,
  such that, following the first occurence of an integer i,
  each subsequent occurrence of i, appears i+1 indices later
  than the last.
  For example, for k=4, a solution would be 41312432
  '''

  * John E. Miller: Langford's Problem
    http://www.lclark.edu/~miller/langford.html

  * https://en.wikipedia.org/wiki/Langford_pairing

  * http://dialectrix.com/langford.html

  * Encyclopedia of Integer Sequences for the number of solutions for each k
    http://www.research.att.com/cgi-bin/access.cgi/as/njas/sequences/eisA.cgi?Anum=014552


  For a solution to be possible this must hold:
      k % 4 == 0 or k % 4 == 3

  Here's a solution of k = 159, solved in 6.6s
  '''
  k: 159
  [127, 137, 2, 116, 75, 2, 89, 86, 151, 15, 119, 123, 28, 79, 112, 156, 148, 130, 
   135, 106, 124, 6, 143, 50, 115, 15, 43, 54, 6, 46, 3, 129, 34, 136, 3, 68, 113, 
   95, 83, 150, 93, 28, 13, 111, 19, 41, 44, 108, 147, 59, 32, 63, 96, 77, 30, 12, 
   13, 21, 73, 153, 53, 121, 155, 31, 19, 131, 39, 34, 12, 67, 43, 1, 5, 1, 50, 36, 
   46, 138, 5, 21, 75, 142, 54, 32, 23, 30, 125, 41, 52, 91, 87, 44, 145, 79, 86, 
   31, 89, 122, 99, 88, 35, 71, 56, 100, 68, 158, 39, 141, 23, 59, 126, 146, 36, 
   57, 53, 63, 27, 97, 134, 157, 116, 120, 83, 149, 109, 84, 106, 112, 127, 144, 
   119, 77, 73, 95, 93, 123, 35, 67, 92, 137, 115, 52, 133, 110, 27, 124, 154, 
   140, 130, 96, 113, 118, 74, 132, 135, 111, 108, 159, 20, 56, 151, 129, 152, 
   139, 76, 148, 143, 128, 8, 51, 136, 57, 156, 71, 55, 60, 114, 8, 87, 20, 45, 
   91, 64, 121, 102, 37, 81, 101, 88, 90, 150, 117, 72, 66, 62, 103, 147, 131, 99, 
   105, 82, 80, 69, 107, 100, 29, 10, 85, 104, 98, 84, 94, 125, 153, 70, 97, 138, 
   10, 155, 58, 122, 51, 22, 37, 142, 78, 45, 74, 4, 49, 55, 92, 17, 4, 109, 29, 
   60, 126, 145, 16, 14, 76, 120, 40, 24, 22, 65, 64, 26, 141, 17, 42, 61, 134, 
   110, 14, 16, 62, 146, 38, 66, 33, 47, 7, 158, 72, 25, 48, 81, 24, 118, 7, 69, 
   149, 144, 26, 133, 157, 58, 49, 90, 18, 80, 82, 40, 70, 132, 102, 140, 101, 11, 
   114, 25, 85, 42, 33, 128, 9, 38, 103, 18, 154, 11, 139, 78, 105, 94, 9, 98, 117, 
   47, 107, 65, 104, 61, 152, 48, 159]
   
  status: FEASIBLE

  NumSolutions: 1
  NumConflicts: 1048
  NumBranches: 206368
  WallTime: 6.595721124000001
  '''

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import ListPrinter


def main(k=8, num_sol=0):

  model = cp.CpModel()

  #
  # data
  #
  print("k:", k)
  
  if not (k % 4 == 0 or k % 4 == 3):
    print("There is no solution for K unless K mod 4 == 0 or K mod 4 == 3")
    return
  p = list(range(2 * k))

  #
  # declare variables
  #
  position = [model.NewIntVar(0, 2 * k - 1, "position[%i]" % i) for i in p]
  solution = [model.NewIntVar(1, k, "position[%i]" % i) for i in p]

  #
  # constraints
  #
  model.AddAllDifferent(position)

  for i in range(1, k + 1):
    model.Add(position[i + k - 1] == position[i - 1] + i + 1)
    model.AddElement(position[i - 1], solution, i)
    model.AddElement(position[k + i - 1], solution, i)

  # symmetry breaking
  model.Add(solution[0] < solution[2 * k - 1])

  #
  # search and result
  #
  solver = cp.CpSolver()

  # Activating these makes it faster for small numbers but
  # slower on larger numbers. 
  # solver.parameters.search_branching = cp.PORTFOLIO_SEARCH
  # solver.parameters.cp_model_presolve = False
  # solver.parameters.linearization_level = 0
  # solver.parameters.cp_model_probing_level = 0

  # status = solver.Solve(model)
  solution_printer = ListPrinter(solution,num_sol)
  # solution_printer = SimpleSolutionCounter(solution) # count sols
  status = solver.SearchForAllSolutions(model, solution_printer)
  print("status:", solver.StatusName(status))
  
  if status != cp.OPTIMAL and status != cp.FEASIBLE:
    print("No solution")


  print()
  print("NumSolutions:", solution_printer.SolutionCount())
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())

def benchmark():
  """
  Benchmark langford for k = 1..200
  """
  for k in range(1,201):
    main(k, 1)
    print()


k = 8
num_sol = 0
if __name__ == "__main__":
  if len(sys.argv) > 1:
    k = int(sys.argv[1])
  if len(sys.argv) > 2:
    num_sol = int(sys.argv[2])

  main(k, num_sol)
  # benchmark()
