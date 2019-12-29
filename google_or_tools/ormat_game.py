# Copyright 2010 Hakan Kjellerstrand hakank@bonetmail.com
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

  Ormat game in Google CP Solver.
  From bit-player "The Ormat Game"
  http://bit-player.org/2010/the-ormat-game
  '''
  I'm going to give you a square grid, with some of the cells colored 
  and others possibly left blank. We'll call this a template. Perhaps 
  the grid will be one of these 3x3 templates:
  
  [see pictures at the web page]
  
  You have a supply of transparent plastic overlays that match the 
  grid in size and shape and that also bear patterns of black dots:
  
  [ibid.]

  Your task is to assemble a subset of the overlays and lay them on 
  the template in such a way that dots cover all the colored squares 
  but none of the blank squares. You are welcome to superimpose multiple 
  dots on any colored square, but overall you want to use as few overlays 
  as possible. To make things interesting, I'll suggest a wager. I'll pay 
  you $3 for a correct covering of a 3x3 template, but you have to pay me 
  $1 for each overlay you use. Is this a good bet?
  '''
  
  This is a prototype which the following limitations:
  - the overlays is not generated dynamically for each n
  - it just shows which overlays that is used. It would be nice
    with a much more graphical output.
  - the questions asked by bit-player is not answered (it requires
    more analysis)

  That said, here is solutions of the three problems stated at the web page
  with minimum number of overlays.
     x is an indicator matrix which overlay to use
     overlay used is a set representation of the overlay used
     num_overlays is the number of overlays used
  

  Compare with the following model:
  * MiniZinc: http://hakank.org/minizinc/ormat_game.mzn

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other Google CP Solver models: http://www.hakank.org/google_or_tools/
  
"""

import string, sys

from ortools.constraint_solver import pywrapcp

#
# Generate all the overlays for a specific size (n)
#
def get_overlays(n=3, debug=0):

    solver = pywrapcp.Solver("Ormat Game")

    #
    # decision variables
    # 
    x = []
    for i in range(n):
        t = []
        for j in range(n):
            t.append(solver.IntVar(0,1, "x[%i][%i]"%(i,j)))
        x.append(t)
    x_flat = [x[i][j] for i in range(n) for j in range(n)]


    #
    # constraints
    #
    for i in range(n):
        solver.Add(solver.Sum([x[i][j] for j in range(n)]) == 1)
        solver.Add(solver.Sum([x[j][i] for j in range(n)]) == 1)

    solution = solver.Assignment()
    solution.Add(x_flat)

    db = solver.Phase(x_flat,
                      solver.INT_VAR_SIMPLE,
                      solver.ASSIGN_MIN_VALUE)
    solver.NewSearch(db)
    num_solutions = 0
    overlays = []
    while solver.NextSolution():
        num_solutions += 1
        ov = [[x[i][j].Value() for i in range(n)] for j in range(n)]
        overlays.append(ov)
    
    return overlays



#
# print a problem
#
def print_problem(problem, n):
    print("Problem:")
    for i in range(n):
        for j in range(n):
            print(problem[i][j], " ",end=" ")
        print('')
    print('')
            

#
# print a solution
#
def print_solution(x, overlays):
    f = len(x)
    n = len(overlays[0])
    print("f:",f, " n: ", n)
    # print("x: ", x)
    print("num overlays:", sum(x))
    for o in range(f):
        if x[o] == 1:
            print("Overlay", o)
            for i in range(n):
                for j in range(n):
                    print(overlays[o][i][j], " ",end=" ")
                print('')
            print('')


def main(problem, overlays, n, debug=0):
    
    # Create the solver.
    solver = pywrapcp.Solver('Ormat Game')

    # data
    f = len(overlays)
    
    # declare variables
    x =  [solver.IntVar(0, 1, "x[%i]" %i) for i in range(f)]
    num_overlays = solver.IntVar(0, f, 'num_overlays')

    y = {}
    for i in range(n):
        for j in range(n):
            y[(i,j)] = solver.IntVar(0,f,"y[%i,%i]"%(i,j))
    y_flat = [y[(i, j)] for i in range(n) for j in range(n)]

    # constraints
    for i in range(n):
        solver.Add(solver.Sum([y[(i,j)] for j in range(n)]) > 0)
        solver.Add(solver.Sum([y[(j,i)] for j in range(n)]) > 0)

    solver.Add(solver.Sum(x) == num_overlays)

    for i in range(n):
        for j in range(n):
            solver.Add(y[(i,j)] == solver.Sum([(x[o])*(overlays[o][i][j]) for o in range(f)]))
            if problem[i][j] == 1:
                solver.Add(y[(i,j)] >= 1)
            if problem[i][j] == 0:
                solver.Add(y[(i,j)] == 0)
                
    # solution and search
    objective = solver.Minimize(num_overlays,1)
    
    db = solver.Phase(x + y_flat,
                      solver.CHOOSE_FIRST_UNBOUND,
                      solver.ASSIGN_MIN_VALUE)
    
    solver.NewSearch(db,[objective])
    num_solutions = 0
    the_solution = []
    while solver.NextSolution():
        t = [x[i].Value() for i in range(f)]
        the_solution = t
        num_solutions += 1
        print("Num overlays: ", sum([x[i].Value() for i in range(f)]))

    print("\nOptimal solution:")
    print_solution(the_solution, overlays)
    print("Num overlays: ", sum(the_solution))
    solver.EndSearch()

    print()
    print("failures:", solver.Failures())
    print("branches:", solver.Branches())
    print("wall_time:", solver.WallTime())
    print()

# problem 1
# n = 3
# problem = [
#     [1,0,0],
#     [0,1,1],
#     [0,1,1]
#     ]

# # Problem grid 2
# n = 3
# problem = [
#     [1,1,1],
#     [1,1,1],
#     [1,1,1]
#     ]


# # Problem grid 3
# n = 3
# problem = [
#     [1,1,1],
#     [1,1,1],
#     [1,1,0]
#     ]


# This rotation of the above works
# n = 3
# problem = [
#     [1,1,1],
#     [1,1,1],
#     [0,1,1]
#     ]



# This is a _bad_ problem since all rows
# and colutions must have at least one cell=1
# n = 3
# problem = [
#     [0,0,0],
#     [0,1,1],
#     [0,1,1]
#     ]


# # Problem grid 4 (n = 4)
# n = 4
# problem = [
#     [1,1,1,1],
#     [1,1,1,1],
#     [1,1,1,1],
#     [1,1,0,0]
#     ]


# variant
# n = 4
# problem = [
#     [1,1,1,1],
#     [1,1,1,1],
#     [1,1,1,1],
#     [1,1,1,0]
#     ]

# variant
n = 4
problem = [
    [1,1,1,1],
     [1,1,1,1],
     [1,1,1,1],
     [1,1,1,1]
     ]



# # Problem grid 5 (n = 5)
# # This is under the section "Out of bounds"
# n = 5
# problem = [
#     [1,1,1,1,1],
#     [1,1,1,1,1],
#     [1,1,1,1,1],
#     [1,1,1,1,1],
#     [1,1,0,0,0]
#     ]

# # Problem grid 6 (n = 6)
# n = 6
# # This is under the section "Out of bounds"%
# problem = [
#     [1,1,1,1,1,1],
#     [1,1,1,1,1,1],
#     [1,1,1,1,1,1],
#     [1,1,1,1,1,1],
#     [1,1,1,1,1,1],
#     [1,1,0,0,0,0]
#     ]


if __name__ == '__main__':
    debug = 1
    print_problem(problem, n)
    overlays = get_overlays(n, debug)
    main(problem, overlays, n, debug)

