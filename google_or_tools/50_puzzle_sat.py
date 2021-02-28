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
  
  Fifty puzzle (Martin Chlond) in OR-tools CP-SAT Solver.

  From Martin Chlond Integer Programming Puzzles:
  http://www.chlond.demon.co.uk/puzzles/puzzles1.html, puzzle nr. 5. 
  Description  : Fifty puzzle
  Source       : The Puzzles of Sam Loyd (P 54)
  '''
  5. A side show at Coney Island is described as follows: "There were ten little 
  dummies which you were to knock over with baseballs. The man said: 'Take as many 
  throws as you like at a cent apiece and stand as close as you please. Add up the 
  numbers on all the men that you knock down and when the sum amounts to exactly 
  fifty, neither more nor less you get a genuine 25 cent Maggie Cline cigar with 
  a gold band around it.'"
  The numbers on the ten dummies were 15, 9, 30, 21, 19, 3, 12, 6, 25, 27. (Loyd)
  '''

  Answer: 6, 19 and 25

  
  This model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OR-tools page: http://www.hakank.org/or_tools/

"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import SimpleSolutionPrinter2


def main():

  model = cp.CpModel()

  n = 10
  v = [3, 6, 9, 12, 15, 19, 21, 25, 27, 30]

  x = [model.NewIntVar(0,1,f"x[{i}]") for i in range(n)]
  sumX = model.NewIntVar(0,n,"sumX")

  model.Add(sumX == sum(x))
  model.Add(sum([v[i]*x[i] for i in range(n)]) == 50)

  solver  = cp.CpSolver()
  solution_printer = SimpleSolutionPrinter2([x,sumX])  
  status = solver.SearchForAllSolutions(model, solution_printer)
  if status in [cp.OPTIMAL, cp.FEASIBLE]:
    print("x:", [v[i] for i in range(n) if solver.Value(x[i]) > 0])
    print("sumX:", solver.Value(sumX))

  print()
  print("NumSolutions:", solution_printer.SolutionCount())  
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())

if __name__ == '__main__':
  main()
