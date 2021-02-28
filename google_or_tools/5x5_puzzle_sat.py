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
  
  5x5 puzzle (Five puzzle, Martin Chlond) in OR-tools CP-SAT Solver.

  From http://www.chlond.demon.co.uk/Five.html (Java applet).
  (Link from http://www.chlond.demon.co.uk/puzzles/puzzles1.html)
  '''
    A B C D E 
    F G H I J 
    K L M N O 
    P Q R S T 
    U V W X Y 

  Each of the squares in the above grid can be in one of two states, lit(white)
  or unlit(red). If the player clicks on a square then that square and each 
  orthogonal neighbour will toggle between the two states. Each mouse click 
  constitutes one move and the objective of the puzzle is to light all 
  25 squares in the least number of moves.
  '''

  (Unfortunately, the links above don't work anymore.)

  This is a port of Martin Chlond's IP model.


  This model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OR-tools page: http://www.hakank.org/or_tools/

"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import ListPrinter

class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, n, x, d, the_sum):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__n = n
        self.__x = x
        self.__d = d
        self.__the_sum = the_sum
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        n = self.__n 
        x = self.__x 
        print("the_sum:", self.Value(self.__the_sum))
        print("x:")
        for i in range(n):
            for j in range(n):
                print(self.Value(x[(i,j)]),end=" ")
            print()
        print()
        
    def SolutionCount(self):
        return self.__solution_count

def main():

  model = cp.CpModel()

  # data
  n = 5

  # decision variabels
  x = {}
  d = {}
  for i in range(n):
    for j in range(n):
      x[(i,j)] = model.NewBoolVar(f"x[{i,j}]")
      d[(i,j)] = model.NewIntVar(0,n,f"d[{i,j}]")
  the_sum = model.NewIntVar(0,n*n*n,"the_sum")
  

  # constraints
  model.Add(the_sum == sum([x[(i,j)] for i in range(n) 
                                      for j in range(n)]))

  for i in range(n):
    for j in range(n):
      model.Add(2*d[(i,j)]+1 ==
                sum([x[(i,k)] for k in range(j-1,j+2) 
                            if k >= 0 and k < n and k != j])
                + 
                sum([x[(k,j)] for k in range(i-1,i+2) 
                            if k >= 0 and k < n])
              )

  solver  = cp.CpSolver()
  
  solution_printer = SolutionPrinter(n, x, d, the_sum)
  status = solver.SearchForAllSolutions(model, solution_printer)
  if not status in [cp.OPTIMAL, cp.FEASIBLE]:
    print("No solution!")

  print()
  print("NumSolutions:", solution_printer.SolutionCount())  
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())
  print()


if __name__ == '__main__':
  main()
