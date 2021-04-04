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
  
  Giant Cat Army riddle in OR-tools CP-SAT Solver.
  Via https://stackoverflow.com/questions/65511714/about-building-a-list-until-it-meets-conditions
  '''
  Basically you start with [0], then you build this list by using one of three 
  operations: adding 5, adding 7, or taking sqrt. You successfully complete the 
  game when you have managed to build a list such that 2,10 and 14 appear 
  on the list, in that order, and there can be other numbers between them.

  The rules also require that all the elements are distinct, they're all <=60 
  and are all only integers. For example, starting with [0], you can
  apply (add5, add7, add5), which would result in [0, 5, 12, 17], but since 
  it doesn't have 2,10,14 in that order it doesn't satisfy the game.
  '''

  There are 99 optimal solutions of length 24. Here is one solution:
     x: [0, 5, 12, 19, 26, 31, 36, 6, 11, 16, 4, 2, 9, 3, 10, 15, 20, 27, 32, 37, 42, 49, 7, 14]
     With the operations:
       0 (+5) 5 (+7) 12 (+7) 19 (+7) 26 (+5) 31 (+5) 36 (//2) 6 (+5) 11 (+5) 
       16 (//2) 4 (//2) 2 (+7) 9 (//2) 3 (+7) 10 (+5) 15 (+5) 20 (+7) 27 (+5) 
       32 (+5) 37 (+5) 42 (+7) 49 (//2) 7 (+7) 14 




  This model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OR-tools page: http://www.hakank.org/or_tools/

"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import SimpleSolutionPrinter2


def main(n=60):

  model = cp.CpModel()

  maxval = 60

  x = [model.NewIntVar(0,maxval,f"x[{i}]") for i in range(n)]
  model.AddAllDifferent(x)

  # Given and symmetry breaking
  model.Add(x[0] == 0)

  # x[1] == 5 || x[1] == 7
  b5 = model.NewBoolVar("b5")
  b7 = model.NewBoolVar("b7")
  model.Add(x[1] == 5).OnlyEnforceIf(b5)
  model.Add(x[1] != 5).OnlyEnforceIf(b5.Not())
  model.Add(x[1] == 7).OnlyEnforceIf(b7)
  model.Add(x[1] != 7).OnlyEnforceIf(b7.Not())
  model.AddBoolOr([b5,b7])

  model.Add(x[n-1] == 14)

  # Encode x[i] == x[i]**2 (i.e. the sqrt operation)
  x2 = [model.NewIntVar(0,maxval**2,f"x2[{i}]") for i in range(maxval)]
  for i in range(n):
    model.AddMultiplicationEquality(x2[i],[x[i],x[i]])
  
  for i in range(n-1):
      # x[i+1] == x[i] + 5 ||
      # x[i+1] == x[i] + 7 ||
      # x[i] == x[i+1]^2
      
      b = [model.NewBoolVar(f"b[{i}]") for i in range(3)]

      # + 5
      model.Add(x[i+1] == x[i] + 5).OnlyEnforceIf(b[0])
      model.Add(x[i+1] != x[i] + 5).OnlyEnforceIf(b[0].Not())

      # + 7
      model.Add(x[i+1] == x[i] + 7).OnlyEnforceIf(b[1])
      model.Add(x[i+1] != x[i] + 7).OnlyEnforceIf(b[1].Not())

      # sqrt (or **2)
      model.Add(x[i] == x2[i+1]).OnlyEnforceIf(b[2])
      model.Add(x[i] != x2[i+1]).OnlyEnforceIf(b[2].Not())

      model.Add(sum(b) == 1) # slightly better
      # model.AddBoolOr(b)


  # 0 .. 2 .. 10 .. 14
  ix2 = model.NewIntVar(1,n,"ix2")
  model.AddElement(ix2,x,2)
  ix10 = model.NewIntVar(1,n,"ix10")
  model.AddElement(ix10,x,10)
  model.Add(ix2 < ix10)

  solver  = cp.CpSolver()
  # solver.parameters.search_branching = cp.PORTFOLIO_SEARCH
  solver.parameters.cp_model_presolve = False # Slightly faster
  # solver.parameters.linearization_level = 0
  solver.parameters.cp_model_probing_level = 0

  status = solver.Solve(model)

  if status in [cp.OPTIMAL, cp.FEASIBLE]:
    print("n:",n)
    xval = [solver.Value(v) for v in x]
    print("x:",xval)
    print("With the operations:")
    for i in range(n):
      if i > 0:
        if xval[i]-xval[i-1] == 5:
          print("(+5)", end=" ")
        elif xval[i]-xval[i-1] == 7:
          print("(+7)", end=" ")
        else: 
          print("(sqrt)", end=" ")
      print(xval[i],end=" ")
    print()
    print()
    print("NumConflicts:", solver.NumConflicts())
    print("NumBranches:", solver.NumBranches())
    print("WallTime:", solver.WallTime())

    return xval
  else:
    return None


if __name__ == '__main__':
  # Try different sequence lengths
  for n in range(5,60):
    sol = main(n)
    if sol != None:
      min_len = n 
      break
