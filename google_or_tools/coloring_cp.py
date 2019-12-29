# Copyright 2013 Hakan Kjellerstrand hakank@gmail.com
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

  Simple coloring problem using MIP in Google CP Solver.

  Problem instance from GLPK:s model color.mod

  Compare with the model:
    http://www.hakank.org/or-tools/coloring_ip.py


  This model was created by Hakan Kjellerstrand (hakank@bonetmail.com)
  Also see my other Google CP Solver models: http://www.hakank.org/google_or_tools/
"""

import sys
from ortools.constraint_solver import pywrapcp


def main():

  solver = pywrapcp.Solver('Map coloring')

  #
  # data
  #

  # max number of colors
  # [we know that 4 suffices for normal, planar, maps]
  nc = 5

  # number of nodes
  n = 11
  # set of nodes
  V = range(n)

  num_edges = 20

  #
  # Neighbours
  #
  # This data correspond to the instance myciel3.col from:
  # http://mat.gsia.cmu.edu/COLOR/instances.html
  #
  # Note: 1-based (adjusted below)
  E =  [[1, 2],
        [1, 4],
        [1, 7],
        [1, 9],
        [2, 3],
        [2, 6],
        [2, 8],
        [3, 5],
        [3, 7],
        [3, 10],
        [4, 5],
        [4, 6],
        [4, 10],
        [5, 8],
        [5, 9],
        [6, 11],
        [7, 11],
        [8, 11],
        [9, 11],
        [10, 11]]


  #
  # decision variables
  #
  x = [solver.IntVar(1, nc, 'x[%i]' % i) for i in V]

  # number of colors used, to minimize
  max_color = solver.Max(x).Var()

  #
  # constraints
  #

  # adjacent nodes cannot be assigned the same color
  # (and adjust to 0-based)
  for i in range(num_edges):
     solver.Add(x[E[i][0]-1] != x[E[i][1]-1])

  # symmetry breaking
  # solver.Add(x[0] == 1);
  # solver.Add(x[1] <= 2);
  for i in range(nc):
      solver.Add(x[i] <= i+1);

  # objective (minimize the number of colors)
  objective = solver.Minimize(max_color, 1)

  #
  # solution
  #
  solution = solver.Assignment()
  solution.Add(x)
  solution.Add(max_color)
  
  db = solver.Phase(x,
                    # solver.CHOOSE_FIRST_UNBOUND,
                    solver.CHOOSE_MIN_SIZE_LOWEST_MAX,
                    
                    # solver.ASSIGN_MIN_VALUE
                    solver.ASSIGN_MIN_VALUE
                    )


  solver.NewSearch(db, [objective])
  num_solutions = 0
  while solver.NextSolution():
    num_solutions += 1
    print("x:", [int(x[i].Value()) for i in V])
    print("max_color:", max_color.Value())
    print()
  
  solver.EndSearch()

  print()
  print("num_solutions:", num_solutions)
  print("failures:", solver.Failures())
  print("branches:", solver.Branches())
  print("WallTime:", solver.WallTime())


if __name__ == '__main__':
    main()
