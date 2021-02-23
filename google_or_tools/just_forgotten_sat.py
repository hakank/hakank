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

  Just forgotten puzzle (Enigma 1517) in OR-tools CP-SAT Solver.

  From http://www.f1compiler.com/samples/Enigma 201517.f1.html
  '''
  Enigma 1517 Bob Walker, New Scientist magazine, October 25, 2008.

  Joe was furious when he forgot one of his bank account numbers.
  He remembered that it had all the digits 0 to 9 in some order,
  so he tried the following four sets without success:

      9 4 6 2 1 5 7 8 3 0
      8 6 0 4 3 9 1 2 5 7
      1 6 4 0 2 9 7 8 5 3
      6 8 2 4 3 1 9 0 7 5

  When Joe finally remembered his account number, he realised that
  in each set just four of the digits were in their correct position
  and that, if one knew that, it was possible to work out his
  account number. What was it?
  '''

  This is port of my old CP model just_forgotten.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import SimpleSolutionCounter

def main():

  model = cp.CpModel()

  #
  # data
  #
  rows = 4
  cols = 10
  # The four tries
  a = [[9, 4, 6, 2, 1, 5, 7, 8, 3, 0], 
       [8, 6, 0, 4, 3, 9, 1, 2, 5, 7],
       [1, 6, 4, 0, 2, 9, 7, 8, 5, 3], 
       [6, 8, 2, 4, 3, 1, 9, 0, 7, 5]]

  #
  # variables
  #
  x = [model.NewIntVar(0, 9, "x[%i]" % j) for j in range(cols)]

  #
  # constraints
  #
  model.AddAllDifferent(x)

  for r in range(rows):
    b = [model.NewBoolVar(f"b[{i}") for i in range(cols)]
    for i in range(cols):
      model.Add(x[i] == a[r][i]).OnlyEnforceIf(b[i])
    model.Add(sum(b) == 4)

  #
  # search and result
  #
  solver = cp.CpSolver() 
  solution_printer = SimpleSolutionCounter(x)
  status = solver.SearchForAllSolutions(model, solution_printer)

  if status == cp.OPTIMAL:
    xval = [solver.Value(x[j]) for j in range(cols)]
    print("Account number:")
    for j in range(cols):
      print("%i " % xval[j], end=" ")
    print()
    print("\nThe four tries, where '!' represents a correct digit:")
    for i in range(rows):
      for j in range(cols):
        check = " "
        if a[i][j] == xval[j]:
          check = "!"
        print("%i%s" % (a[i][j], check), end=" ")
      print()
    print()
  print()

  print("NumSolutions:", solution_printer.SolutionCount())
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


if __name__ == "__main__":
  main()
