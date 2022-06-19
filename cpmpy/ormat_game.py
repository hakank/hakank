"""
Ormat game in cpmpy.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

#
# Generate all the overlays for a specific size (n).
#
def get_overlays(n=3, debug=0):

    x = boolvar(shape=(n, n),name="x")
    model = Model (
        [ sum(row) == 1 for row in x],
        [ sum(col) == 1 for col in x.transpose()]        
        )

    # all solution solving, with blocking clauses
    ss = CPM_ortools(model)
    # s.ort_solver.parameters.num_search_workers = 8 # not for SearchForAllSolutions!
    # s.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH 
    # s.ort_solver.parameters.cp_model_presolve = False
    ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0

    print("\nFind overlay ", end="",flush=True)

    overlays = []
    def print_sol():
      overlays.append(x.value())

    ss.solveAll(display=print_sol)

    return overlays



# Generate all the problems of size n
def all_problems(n = 3, debug = 0):
    x = boolvar(shape=(n, n),name="x")

    model = Model (
        
        [ sum(row) >= 1 for row in x],
        [ sum(col) >= 1 for col in x.transpose()], 
        )

    problems = []
    ss = CPM_ortools(model)
    while ss.solve(): 
        if debug:
            print("x2:",x)
        problems.append([[x[i,j].value() for i in range(n)] for j in range(n) ])

    return problems
    

# print a solution
def print_solution(x, overlays):
    f = len(x)
    n = len(overlays[0])
    print("f:",f, " n: ", n)
    for o in range(f):
        if x[o].value() == 1:
            print("overlay", o)
            for i in range(n):
                for j in range(n):
                    print( overlays[o][i][j], " ",end="")
                print()
            print()


# print a problem
def print_problem(problem, n):
    print("Problem:")
    for i in range(n):
        for j in range(n):
            print(problem[i][j], end=" ")
        print()
    print()
            

#
# This solves a problem instance
#
def ormat_game(problem, overlays, n, debug=0):

    f = len(overlays)
    x = boolvar(shape=f, name="x")
    num_overlays = intvar(0,f,name="num_overlays")

    # Count the number of occurrences for each cell for
    # the choosen overlays.
    # Mainly for debugging purposes, but it also makes the
    # modeling easier.
    y = intvar(0,f,shape=(n,n),name="y")
    y_flat = y.flat
    
    model = Model (
        # sanity clauses
        [ sum(row) >= 1 for row in y],
        [ sum(col) >= 1 for col in y.transpose()],         
        num_overlays == sum(x),
        minimize=num_overlays,
        )

    for i in range(n):
        for j in range(n):           
            model += [y[i,j] == sum([(x[o])*(overlays[o][i][j]) for o in range(f)])]

            # model += [y[i,j] >= problem[i][j]]
            
            if problem[i][j] == 1:
                model += [y[i,j] >= 1]

            if problem[i][j] == 0:
                model += [y[i,j] == 0]

    if debug:
        print(model)

     
    print("Solve")

    
    # all solution solving, with blocking clauses
    s = CPM_ortools(model)

    # s.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH 
    # s.ort_solver.parameters.cp_model_presolve = False
    s.ort_solver.parameters.linearization_level = 0
    s.ort_solver.parameters.cp_model_probing_level = 0

    num_solutions = 0
    if s.solve(num_search_workers=12):
        num_solutions += 1
        print("x:\n",x.value())
        print("overlays:", [i for i in range(f) if x[i].value() == 1])
        print("y:\n",y.value())
        print("num_overlays:", num_overlays.value())
        print_solution(x, overlays)

    print("Num conflicts:", s.ort_solver.NumConflicts())
    print("NumBranches:", s.ort_solver.NumBranches())
    print("WallTime:", s.ort_solver.WallTime())


problems = {
    "problem1": {
    "n": 3,
    "problem": [
               [1,0,0],
               [0,1,1],
               [0,1,1]
               ]
    },

    # Problem grid 2
    "problem2" : {
    "n": 3,
    "problem" : [
               [1,1,1],
               [1,1,1],
               [1,1,1]
               ]
    },

    "problem3": {
    "n": 3,
    "problem": [
              [1,1,1],
              [1,1,1],
              [0,1,1]
              ]
    },


# Note: Before solve y matrix has y2.2 in {} which is very bad, and
#       is the reason (or a symptom) that this problem shows no solution.
#
# x before solve: [x0 in {0,1}, x1 in {1}, x2 in {0,1}, x3 in {1}, x4 in {1}, x5 in {0,1}]
# y before solve:
# [[y0.0 in {0..2}, y0.1 in {0,1}, y0.2 in {0..3}],
# [y1.0 in {0..3}, y1.1 in {0..2}, y1.2 in {0,1}],
# [y2.0 in {0,1}, y2.1 in {0..3}, y2.2 in {}]]
#
# Strange: another run has not this empty domain for y2.2...
#
# # Problem grid 3
    "problem4": {
    "n":3,
    "problem":  [
    [1,1,1],
    [1,1,1],
    [1,1,0]
    ]

  },

# This rotation of the above works
  "problem5": {
    "n": 3,
    "problem": [
    [1,1,1],
    [1,1,1],
    [0,1,1]

    ]
    },

# This is a _bad_ problem since all rows
# and colutions must have at least one cell=1
   "problem6": {
    "n": 3,
    "problem" : [
    [0,0,0],
    [0,1,1],
    [0,1,1]
    ]
},


# # Problem grid 4 (n = 4)
"problem7": {
"n" : 4,
"problem" : [
    [1,1,1,1],
    [1,1,1,1],
    [1,1,1,1],
    [1,1,0,0]
    ]
},


# variant
"problem8": {
"n" : 4,
"problem" : [
    [1,1,1,1],
    [1,1,1,1],
    [1,1,1,1],
    [1,1,1,0]
    ]
},

# variant
"problem9" : {
"n":4,
"problem" : [
    [1,1,1,1],
     [1,1,1,1],
     [1,1,1,1],
     [1,1,1,1]
     ]
},


# # Problem grid 5 (n = 5)
# # This is under the section "Out of bounds"
"problem10": {
"n" : 5,
"problem" : [
    [1,1,1,1,1],
    [1,1,1,1,1],
    [1,1,1,1,1],
    [1,1,1,1,1],
    [1,1,0,0,0]
    ]
},

# # Problem grid 6 (n = 6)
"problem11" : {
"n": 6,
# This is under the section "Out of bounds"%
"problem" : [
    [1,1,1,1,1,1],
    [1,1,1,1,1,1],
    [1,1,1,1,1,1],
    [1,1,1,1,1,1],
    [1,1,1,1,1,1],
    [1,1,0,0,0,0]
    ]
},

}


debug = 0
for p in problems:
    print(f"\nproblem {p}")    
    problem = problems[p]["problem"]
    n = problems[p]["n"]
    print_problem(problem, n)
    print("Generating overlays ... ", end = " ",flush=True)
    overlays = get_overlays(n, debug)
    print("ok")
    print("Number of candidate overlays: ", len(overlays))
    # print("overlays:", overlays)
    ormat_game(problem, overlays, n, debug)


## Test all problems of a certain size
# n=3
# debug = 0
# num_problems = 0
# num_solved = 0
# overlays = get_overlays(n, debug)
# problems = all_problems(n, debug)
# print("num_problems:", len(problems))
# for p in problems:
#     print_problem(p,n)
#     num_problems += 1
#     num_sol = ormat_game(p, overlays, n, debug)
#     num_solved += num_sol
#     # print("num_sol:",num_sol)

# print("num_problems: ", num_problems)
# print("num_solved:", num_solved)
