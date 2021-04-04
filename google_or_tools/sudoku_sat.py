# Copyright 2021 Hakan Kjellerstrand hakank@gmail.com
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

  Sudoku solver in OR-tools CP-SAT Solver.

  This Sudoku solver is based on sudoku.py from the OR-tools CP distribution
  and is somewhat alterered.

  See sudoku_problems.py for the instances which was taken from my Picat model
  http://hakank.org/picat/sudoku.pi . Mostly it is from the Gecode's collection
  of Sudoku instances of size 9x9, 16x16, and 25x25: 
  http://www.gecode.org/gecode-doc-latest/sudoku08cpp-source.html


  All 9x9 and 16x16 are solved immediately with this solver, so the interesting 
  instances are the 25x25 instances. 

  Below are times both for first solution and the of proving unicity of 
  the solution, i.e. check for 2 solutions. This is slower by about 
  a factor of 2. We experiment with some parameter settings:
    
    solver.parameters.linearization_level = 0
    solver.parameters.cp_model_probing_level = 0

  And for finding the first solution (since we can't use more than 1
  worker to find parallize search):
    solver.parameters.num_search_workers = 8


  Only first solution (not proving unicity)
  -----------------------------------------
  * Total time 1min28.60s
    solver.parameters.linearization_level = 0
    solver.parameters.cp_model_probing_level = 0

  * Total time 1min52.32s
    # solver.parameters.linearization_level = 0
    # solver.parameters.cp_model_probing_level = 0

  * Total time 1min23.21s
    # solver.parameters.linearization_level = 0
    solver.parameters.cp_model_probing_level = 0

  * Total time:  50.8s
    solver.parameters.num_search_workers = 8
    # solver.parameters.linearization_level = 0
    solver.parameters.cp_model_probing_level = 0


  Proving Unicity
  ----------------
  * Total time:  3min43.55s
    # solver.parameters.linearization_level = 0
    # solver.parameters.cp_model_probing_level = 0

  * Total time 3min01.38s
    solver.parameters.linearization_level = 0
    solver.parameters.cp_model_probing_level = 0

  * Total time 3min01.38s
    # solver.parameters.linearization_level = 0
    solver.parameters.cp_model_probing_level = 0

  * Total time  2min59.94s
    solver.parameters.linearization_level = 0
    # solver.parameters.cp_model_probing_level = 0


  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or-tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *
from sudoku_problems import all_sudoku_problems


class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, line, grid, num_sols=2):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__line = line
        self.__grid = grid
        self.__num_sols = num_sols
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        for i in self.__line:
          print([self.Value(self.__grid[(i, j)]) for j in self.__line])
        print()
        if self.__solution_count >= self.__num_sols:
          # print("We found a second solution. Skip the rest.")
          self.StopSearch()

    def SolutionCount(self):
        return self.__solution_count


def sudoku(initial_grid,print_sol=1,num_sols=2,num_workers=1):

  model = cp.CpModel()  
  n = len(initial_grid[0])
  
  cell_size = math.ceil(math.sqrt(n))
  line_size = cell_size ** 2
  line = range(0, line_size)
  cell = range(0, cell_size)

  grid = {}
  for i in line:
    for j in line:
      grid[(i, j)] = model.NewIntVar(1, line_size, 'grid %i %i' % (i, j))

  # AllDifferent on rows.
  for i in line:
    model.AddAllDifferent([grid[(i, j)] for j in line])

  # AllDifferent on columns.
  for j in line:
    model.AddAllDifferent([grid[(i, j)] for i in line])

  # AllDifferent on cells.
  for i in cell:
    for j in cell:
      one_cell = []
      for di in cell:
        for dj in cell:
          one_cell.append(grid[(i * cell_size + di, j * cell_size + dj)])

      model.AddAllDifferent(one_cell)

  # Initial values.
  for i in line:
    for j in line:
      if initial_grid[i][j]:
        model.Add(grid[(i, j)] == initial_grid[i][j])

  solver = cp.CpSolver()
  if num_workers > 1:
    solver.parameters.num_search_workers = num_workers

  # solver.parameters.search_branching = cp.PORTFOLIO_SEARCH
  # solver.parameters.cp_model_presolve = False
  solver.parameters.linearization_level = 0
  # solver.parameters.cp_model_probing_level = 0

  solution_printer = SolutionPrinter(line, grid,num_sols)
  if num_workers > 1:
    status = solver.SolveWithSolutionCallback(model,solution_printer)
  else:
    status = solver.SearchForAllSolutions(model,solution_printer)

  if not status in [cp.OPTIMAL,cp.FEASIBLE]:
      print("No solution.")
    

  if print_sol:
    print()
    print("NumConflicts:", solver.NumConflicts())
    print("NumBranches:", solver.NumBranches())
    print("WallTime:", solver.WallTime())
    print()

  return solver.WallTime()

def run_sudoku_problems(size=9,print_sols=1,num_sols=2,num_workers=1):
  """
  run_sudoku_problems(size=9)

  Run all Sudoku problems of size `size`.

  Returns a list of the wall times.
  """
  times = []
  for p in sorted(all_sudoku_problems.keys()):
    prob = all_sudoku_problems[p]
    if len(prob[0])== size:
      print(f"\nRun #{p}:")
      t = sudoku(prob,print_sols,num_sols,num_workers)
      print(f"Problem #{p}:", t)
      times.append((p,size,t))
  return times


print_sols = 1
num_sols = 2 # 2: Prove unicity  1: Just the first solution
num_workers = 1 # It > 1 it only give the first solution.
if __name__ == '__main__':
  all_times = []
  print("\n\nSize 9:")
  times9 = run_sudoku_problems(9,print_sols,num_sols,num_workers)
  for pt in times9:
    all_times.append(pt)

  print("\n\nSize 16:")
  times16 = run_sudoku_problems(16,print_sols,num_sols,num_workers)
  for pt in times16:
    all_times.append(pt)

  print("\n\nSize 25:")
  times25 = run_sudoku_problems(25,print_sols,num_sols,num_workers)
  for pt in times25:
    all_times.append(pt)

  for pt in all_times:
    print(pt)
