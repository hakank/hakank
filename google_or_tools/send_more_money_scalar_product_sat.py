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

  SEND+MORE=MONEY in 'any' base in OR-tools CP-SAT Solver.

  Alphametic problem SEND+MORE=MONEY in any base.

  Examples:
  Base 10 has one solution:
     {9, 5, 6, 7, 1, 0, 8, 2}
  Base 11 has three soltutions:
     {10, 5, 6, 8, 1, 0, 9, 2}
     {10, 6, 7, 8, 1, 0, 9, 3}
     {10, 7, 8, 6, 1, 0, 9, 2}


  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/

"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import ListPrinter

import sys
import string

def main(base=10):

    model = cp.CpModel()

    # data

    # declare variables
    x = [model.NewIntVar(0,base-1,f"x[{i}") for i in range(8)]
    s,e,n,d,m,o,r,y = x
    xx = [s,e,n,d,  m,o,r,e, m,o,n,e,y]
    coeffs =  [1000, 100, 10, 1,         # S E N D +
               1000, 100, 10, 1,         # M O R E
               -10000,-1000, -100,-10,-1 # == M O N E Y
               ]

    #
    # constraints
    #
    model.AddAllDifferent(x)
    model.Add(0 == cp.LinearExpr.ScalProd(xx, coeffs))
    model.Add(s > 0)
    model.Add(m > 0)

    #
    # solution and search
    #
    solver = cp.CpSolver()
    solution_printer = ListPrinter(x)
    status = solver.SearchForAllSolutions(model, solution_printer)
    
    if status != cp.OPTIMAL:
        print("No solution!")
        
    print()
    print("NumConflicts:", solver.NumConflicts())
    print("NumBranches:", solver.NumBranches())
    print("WallTime:", solver.WallTime())
    print()


base = 10
if __name__ == '__main__':
    # for base in range(10,30):
    #    main(base)
    if len(sys.argv) > 1:
        base=int(sys.argv[1])

    main(base)
