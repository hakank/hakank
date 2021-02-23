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

  Broken weights problem in Google OR-tools CP-SAT Solver.

  From http://www.mathlesstraveled.com/?p=701
  '''
  Here's a fantastic problem I recently heard. Apparently it was first
  posed by Claude Gaspard Bachet de Meziriac in a book of arithmetic problems
  published in 1612, and can also be found in Heinrich Dorrie's 100
  Great Problems of Elementary Mathematics.

      A merchant had a forty pound measuring weight that broke
      into four pieces as the result of a fall. When the pieces were
      subsequently weighed, it was found that the weight of each piece
      was a whole number of pounds and that the four pieces could be
      used to weigh every integral weight between 1 and 40 pounds. What
      were the weights of the pieces?

  Note that since this was a 17th-century merchant, he of course used a
  balance scale to weigh things. So, for example, he could use a 1-pound
  weight and a 4-pound weight to weigh a 3-pound object, by placing the
  3-pound object and 1-pound weight on one side of the scale, and
  the 4-pound weight on the other side.
  '''

  This is a port of my old OR-tools CP model broken_weights.py


  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other Google OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import SimpleSolutionPrinter


def main(m=40, n=4):

  model = cp.CpModel()

  
  # data
  print('total weight (m):', m)
  print('number of pieces (n):', n)
  print()

  # variables
  weights = [model.NewIntVar(1, m, 'weights[%i]' % j) for j in range(n)]
  x = {}
  for i in range(m):
    for j in range(n):
      x[(i, j)] = model.NewIntVar(-1, 1, 'x[%i,%i]' % (i, j))

  # constraints

  # symmetry breaking
  for j in range(1, n):
    model.Add(weights[j - 1] < weights[j])

  model.Add(m == sum(weights))

  # Check that all weights from 1 to 40 can be made.
  #
  # Since all weights can be on either side
  # of the side of the scale we allow either
  # -1, 0, or 1 or the weights, assuming that
  # -1 is the weights on the left and 1 is on the right.
  #
  for i in range(m):
    s = [model.NewIntVar(-m,m, f"s[{i}") for i in range(n)]  
    for j in range(n):
      model.AddMultiplicationEquality(s[j], [weights[j],x[(i,j)]])
    model.Add(i + 1 == sum(s))

  # objective
  model.Minimize(weights[n - 1])
  
  #
  # search and result
  #
  solver = cp.CpSolver() 

  # These speeds it up considerably!
  solver.parameters.search_branching = cp.PORTFOLIO_SEARCH
  solver.parameters.cp_model_presolve=False
  solver.parameters.linearization_level = 0
  solver.parameters.cp_model_probing_level = 0


  # status = solver.Solve(model)
  solution_printer = SimpleSolutionPrinter(weights)
  status = solver.SolveWithSolutionCallback(model, solution_printer)

  print("status:",status)
  num_solutions = 0
  if status == cp.OPTIMAL:
    num_solutions += 1
    print('weights:   ', end=' ')
    for w in [solver.Value(weights[j]) for j in range(n)]:
      print('%3i ' % w, end=' ')
    print()
    print('-' * 30)
    for i in range(m):
      print('weight  %2i:' % (i + 1), end=' ')
      for j in range(n):
        print('%3i ' % solver.Value(x[i, j]), end=' ')
      print()
    print()
  print()


  print('num_solutions:', num_solutions)
  print('NumConflicts :', solver.NumConflicts())
  print('NumBranches :', solver.NumBranches())
  print('WallTime:', solver.WallTime(), 'ms')


m = 40
n = 4
if __name__ == '__main__':
  if len(sys.argv) > 1:
    m = int(sys.argv[1])
  if len(sys.argv) > 2:
    n = int(sys.argv[2])
  main(m, n)
