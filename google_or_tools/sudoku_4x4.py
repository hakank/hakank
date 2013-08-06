# Copyright 2010-2011 Google
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
This model implements a sudoku solver.

This is a very simple 4x4 problem instance:
Problem 26: Shidoku from
 4 _  _ _
 3 1  _ _
 
 _ _  4 1
 _ _  _ 2

This model is based on the sudoku.py model
in the or-tools/python directory
"""



from constraint_solver import pywrapcp

def main():

  # For profiling
  #solver_parameters=pywrapcp.SolverParameters()
  #solver_parameters.profile_level = pywrapcp.SolverParameters.NORMAL_PROFILING


  # Create the solver.
  # solver = pywrapcp.Solver('sudoku', solver_parameters)
  solver = pywrapcp.Solver('sudoku')
  block_size = 2
  line_size = block_size ** 2
  line = range(0, line_size)
  block = range(0, block_size)

  initial_grid = [[4, 0,  0, 0],
                  [3, 1,  0, 0],
                  
                  [0, 0,  4, 1],
                  [0, 0,  0, 2]]

  grid = {}
  for i in line:
    for j in line:
      grid[(i, j)] = solver.IntVar(1, line_size, 'grid %i %i' % (i, j))

  # AllDifferent on rows.
  for i in line:
    solver.Add(solver.AllDifferent([grid[(i, j)] for j in line]))

  # AllDifferent on columns.
  for j in line:
    solver.Add(solver.AllDifferent([grid[(i, j)] for i in line]))

  # AllDifferent on blocks.
  for i in block:
    for j in block:
      one_block = []
      for di in block:
        for dj in block:
          one_block.append(grid[(i * block_size + di, j * block_size + dj)])

      solver.Add(solver.AllDifferent(one_block))

  # Initial values.
  for i in line:
    for j in line:
      if initial_grid[i][j]:
        solver.Add(grid[(i, j)] == initial_grid[i][j])

  all_vars = [grid[(i, j)] for i in line for j in line]

  db = solver.Phase(all_vars,
                    solver.INT_VAR_SIMPLE,
                    solver.INT_VALUE_SIMPLE)

  # And solve.
  solver.NewSearch(db)

  while solver.NextSolution():
    for i in line:
      print [int(grid[(i, j)].Value()) for j in line]

  print
  print "num_solutions:", solver.Solutions()
  print "failures:", solver.Failures()
  print "branches:", solver.Branches()
  print "wall_time:", solver.WallTime()


if __name__ == '__main__':
  main()
