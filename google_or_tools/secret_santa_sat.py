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
  Secret Santa problem in OR-tools CP-SAT Solver.

  From Ruby Quiz Secret Santa
  http://www.rubyquiz.com/quiz2.html
  '''
  Honoring a long standing tradition started by my wife's dad, my friends
  all play a Secret Santa game around Christmas time. We draw names and
  spend a week sneaking that person gifts and clues to our identity. On the
  last night of the game, we get together, have dinner, share stories, and,
  most importantly, try to guess who our Secret Santa was. It's a crazily
  fun way to enjoy each other's company during the holidays.

  To choose Santas, we use to draw names out of a hat. This system was
  tedious, prone to many 'Wait, I got myself...' problems. This year, we
  made a change to the rules that further complicated picking and we knew
  the hat draw would not stand up to the challenge. Naturally, to solve
  this problem, I scripted the process. Since that turned out to be more
  interesting than I had expected, I decided to share.

  This weeks Ruby Quiz is to implement a Secret Santa selection script.

  Your script will be fed a list of names on STDIN.
  ...
  Your script should then choose a Secret Santa for every name in the list.
  Obviously, a person cannot be their own Secret Santa. In addition, my friends
  no longer allow people in the same family to be Santas for each other and your
  script should take this into account.
  '''

  Comment: This model skips the file input and mail parts. We
           assume that the friends are identified with a number from 1..n,
           and the families is identified with a number 1..num_families.


  This is a port of my old CP model secret_santa.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models:
  http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
from cp_sat_utils import ListPrinter

def main():

  model = cp.CpModel()

  #
  # data
  #
  family = [1, 1, 1, 1, 2, 3, 3, 3, 3, 3, 4, 4]
  n = len(family)

  #
  # declare variables
  #
  x = [model.NewIntVar(0, n - 1, 'x[%i]' % i) for i in range(n)]

  #
  # constraints
  #
  model.AddAllDifferent(x)

  # Can't be one own's Secret Santa
  # Ensure that there are no fix-point in the array
  for i in range(n):
    model.Add(x[i] != i)

  # No Secret Santa to a person in the same family
  for i in range(n):
    val = model.NewIntVar(min(family),max(family), "val")
    model.AddElement(x[i], family, val)
    model.Add(val != family[i])

  #
  # solution and search
  #
  solver = cp.CpSolver()
  # solver.parameters.search_branching = cp.PORTFOLIO_SEARCH
  # solver.parameters.cp_model_presolve = False
  # solver.parameters.linearization_level = 0
  # solver.parameters.cp_model_probing_level = 0

  solution_printer = ListPrinter(x)
  # solution_printer = SimpleSolutionCounter(x)
  status = solver.SearchForAllSolutions(model, solution_printer)

  if not (status == cp.OPTIMAL or status == cp.FEASIBLE):
    print("No solution!")


  print('NumSolutions:', solution_printer.SolutionCount())
  print('NumConflicts:', solver.NumConflicts())
  print('NumBranches:', solver.NumBranches())
  print('WallTime:', solver.WallTime())


if __name__ == '__main__':
  main()
