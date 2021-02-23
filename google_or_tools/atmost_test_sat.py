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

  Test of atmost, atleast, and exactly in Google CP-SAT Solver.


  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other Google OR-Tools models: http://www.hakank.org/or_tools/

"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import atmost, atleast, exactly


class SolutionPrinter(cp.CpSolverSolutionCallback):
    def __init__(self, n, x, val):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__n = n
        self.__x = x
        self.__val = val
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        print("x:", [self.Value(self.__x[i]) for i in range(self.__n)], "val (3 occ.):", self.Value(self.__val))

    def SolutionCount(self):
        return self.__solution_count



def main():
    
    model = cp.CpModel()


    n = 5
    

    # declare variables
    x = [model.NewIntVar(0, n,"x%i"%i) for i in range(n)]
    val = model.NewIntVar(0,n, "val")

    #
    # constraints
    #
    
    # There must be at most 1 occurrences of 2
    atmost(model, 2, x, 1)

    # There must be at least 2 occurrences of 1
    atleast(model, 1, x, 2)

    # What value has exactly 3 occurrences?
    exactly(model, val, x, 3)

    
    #
    # Search and solution
    #
    solver = cp.CpSolver() 
    solution_printer = SolutionPrinter(n,x,val)
    status = solver.SearchForAllSolutions(model,solution_printer)
    # status = solver.Solve(model)

    # if status == cp.OPTIMAL:
    #   print("x:", [solver.Value(x[i]) for i in range(n)])
    #   print("val:", solver.Value(val))
    #   print()
    print()
    print("NumSolutions:", solution_printer.SolutionCount())
    print("NumConflicts:", solver.NumConflicts())
    print("NumBranches:", solver.NumBranches())
    print("WallTime:", solver.WallTime())

if __name__ == '__main__':
    main()
