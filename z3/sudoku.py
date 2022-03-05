#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Sudoku in Z3.
#
# SolverFor("QF_LIA")
# Here are the times for first solution (i.e. prove_unicity = False):
#  - world_hardest (9x9):   0.178s
#  - another (9x9)      :   0.05s
#  - problem_34 (16x16) :   0.297s
#  - problem_89 (25x25) : 166.05s
#
# Here are the times for prove_unicity = True:
#  - world_hardest (9x9):   0.368s
#  - another (9x9)      :   0.093s
#  - problem_34 (16x16) :   0.69s
#  - problem_89 (25x25) : 465.17s
#
#
# QF_FD is slower than QF_LIA for finding first solution
# but faster for proving unicity.
# Time to first solution:
#  - world_hardest (9x9):   0.10s
#  - another (9x9)      :   0.049s
#  - problem_34 (16x16) :   0.338s
#  - problem_89 (25x25) : 205.10s
#
# Proving unicity:
#  - world_hardest (9x9):   0.136s
#  - another (9x9)      :   0.06s
#  - problem_34 (16x16) :   0.439s
#  - problem_89 (25x25) : 318.21s
#
#
# Compare with sudoku_ip.py that uses (pseuso) boolean with AtLeast and AtMost constraints
# which is slower on easy cases (9x9 and 16x16) but significantly faster on the 25x25 case:
# It solves problem 89 (25x25) in 3.87 for first solution and 5,49s proving unicity.
# 

# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
#
import time
from z3_utils_hakank import *

def sudoku(init,m=3,prove_unicity=True):
    
    # sol = Solver()
    # sol = SimpleSolver()
    # sol = SolverFor("QF_LIA")
    if prove_unicity:
        sol = SolverFor("QF_FD")
    else:
        sol = SolverFor("LIA")
        
    
    n = m ** 2
    line = list(range(0, n))
    cell = list(range(0, m))

    # Setup
    x = {}
    for i in line: 
        for j in line:
            x[(i,j)] = Int('x %i %i' % (i,j))
            sol.add(x[(i,j)] >= 1, x[(i,j)] <= m*m)

    # Distinct on rows and columns
    for i in line:
        sol.add(Distinct([x[(i,j)] for j in line]))
        sol.add(Distinct([x[(j,i)] for j in line]))
    
    # Distinct on cells.
    for i in cell:
        for j in cell:
            this_cell = []
            for di in cell:
                for dj in cell:
                    this_cell.append(x[(i * m + di, j * m + dj)])
            sol.add(Distinct(this_cell))

    # Init grid
    for i in line:
        for j in line:
            if init[i][j]:
                sol.add(x[(i, j)] == init[i][j])

    # Check for unicity
    # (for just the first solution: change "while" to "if")
    if prove_unicity:
        while sol.check() == sat:
            mod = sol.model()
            print_grid(mod,x,n,n)
            print()
            getDifferentSolutionMatrix(sol,mod,x, n,n)
    else:
        if sol.check() == sat:        
            mod = sol.model()
            print_grid(mod,x,n,n)

# Problem from
# "World's hardest sudoku: can you crack it?"
# http://www.telegraph.co.uk/science/science-news/9359579/Worlds-hardest-sudoku-can-you-crack-it.html
#
# Note: Null time (i.e. just reading this model w/o any goal): 0.204s
# 
world_hardest = [[8,0,0, 0,0,0, 0,0,0],
                 [0,0,3, 6,0,0, 0,0,0],
                 [0,7,0, 0,9,0, 2,0,0],

                 [0,5,0, 0,0,7, 0,0,0],
                 [0,0,0, 0,4,5, 7,0,0],
                 [0,0,0, 1,0,0, 0,3,0],

                 [0,0,1, 0,0,0, 0,6,8],
                 [0,0,8, 5,0,0, 0,1,0],
                 [0,9,0, 0,0,0, 4,0,0]]

# From https://ericpony.github.io/z3py-tutorial/guide-examples.htm
# (another Z3 Sudoku model)
# That z3 model solves this problem in 0.29s
# This model: 0.04s (0.06s to prove uniciy)
another = [[0,0,0,0,9,4,0,3,0],
           [0,0,0,5,1,0,0,0,7],
           [0,8,9,0,0,0,0,4,0],
           [0,0,0,0,0,0,2,0,8],
           [0,6,0,2,0,1,0,5,0],
           [1,0,2,0,0,0,0,0,0],
           [0,7,0,0,0,0,5,2,0],
           [9,0,0,0,6,5,0,0,0],
           [0,4,0,9,7,0,0,0,0]]


# 
# This problem is problem 34 from
# Gecode's sudoku.cpp
# http://www.gecode.org/gecode-doc-latest/sudoku_8cpp-source.html
#
# Size : 16 x 16
#
problem_34 = [[13, 9, 2, 0, 0, 0, 0, 0,16, 0, 0, 0, 4, 3, 0, 0],
              [ 4,12,15, 0, 0, 0, 0, 0, 9,13, 0, 2, 0, 6,14,11],
              [ 0,14, 0, 1, 0, 0, 0, 0,15, 0, 8,11,12, 0, 0,10],
              [16, 5, 6, 0, 0, 0, 0, 0,10, 3,12, 0, 0, 0, 0, 1],
              [ 0, 7,16, 5,10, 8, 0, 0, 0, 0, 6, 1, 0, 0, 0, 0],
              [ 2, 0, 0, 0,12, 0, 0, 0, 0,11, 7, 0, 0, 0, 0, 0],
              [ 0, 0,10,14, 0, 9, 6, 4, 0, 0,16, 0, 0, 0, 0, 0],
              [ 0,15, 9, 0, 5, 0, 7, 0, 4, 0, 0, 0, 0, 0, 0, 0],
              [ 0, 0, 0, 0, 0, 2, 9, 0, 0, 0, 0,10, 0,12, 0, 0],
              [ 0, 0, 0, 0, 0, 0, 0, 0, 6, 4, 5,13, 0, 1, 0, 0],
              [ 0, 0, 0, 0,13, 0, 0, 0, 0, 1, 0,12, 0,11, 7,15],
              [ 0, 0, 0, 0, 0,14, 0,12, 2,16, 0, 0, 0, 8,10, 9],
              [11, 0, 0, 9, 0,16, 5, 2, 0, 0, 0, 0, 0,14,15, 6],
              [ 0, 2, 5, 6, 0, 0,15, 0, 0, 0, 0, 0,13, 0,11, 0],
              [14, 1, 3, 0, 6, 0,13, 0, 0, 0, 0, 0, 0, 0, 0, 7],
              [10, 0, 0, 0, 8,11,12, 3, 0, 0, 0, 0, 9, 5, 4, 0]]


# 
# This problem is problem 89 from
# Gecode's sudoku.cpp
# http://www.gecode.org/gecode-doc-latest/sudoku_8cpp-source.html
#
# Size : 25 x 25
#
problem_89 = [[11,23,13,10,19,16, 6, 2,24, 7, 5, 9, 1,20,17,15, 8,18,25, 3, 4,12,21,22,14],
              [15,16, 0,22, 0,11, 8, 0, 0, 0,25, 0,14, 0, 0, 0,12,19, 0, 0,17, 0, 0, 0, 0],
              [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,16, 0, 4, 0,17, 0,13, 0,24, 0,23,19,10, 2],
              [ 0, 0, 0, 0, 0,19, 0,14,23, 4, 0,21, 6,22,10, 0,11, 0, 2, 0, 0, 0, 0, 0, 0],
              [17,14, 0, 0, 2, 0, 0,13,12, 0, 0, 0, 0, 0,15, 4,20,22,10, 0,11, 0, 9,24, 8],
              [22, 0, 0, 0, 0, 6, 2, 0, 0, 0, 4, 7,12, 1, 9, 0, 0, 0, 0, 0, 0,14, 5, 0, 0],
              [ 0,18, 2, 0, 8,22, 0,19,16,21, 0, 0, 0,10,13,23, 0, 0,20, 0, 0, 3, 0,15, 7],
              [ 0, 0,17, 3, 0, 5, 0, 0, 8, 9, 0, 0, 0, 0,18, 0,19, 0, 0, 0, 0, 0,23,21, 0],
              [ 1,11, 0, 0, 9, 0,15,10,25, 0, 6, 0,23, 0, 0, 0, 0, 5, 3, 7, 0,17, 0, 0,24],
              [ 0, 0, 0, 0, 0, 0, 1, 0, 0,23, 0, 0, 0,24, 0, 0, 0,21,12, 0, 6, 8, 0,25,16],
              [20,24,10, 0,15,23,11,17, 0, 0, 0, 0, 0, 7, 0,12, 0, 0, 0, 0, 0,22, 0, 0, 6],
              [ 4, 5, 0,14,12,25, 0,18, 0, 0,23, 0,15, 0,19, 1, 0, 0, 0,22,20, 0, 7, 9, 0],
              [18, 0,21, 0, 0, 8, 0,24, 0, 0, 9, 0,25, 0, 0, 0,10, 0, 0, 0, 2, 0, 1,19, 0],
              [ 0, 0, 6, 2, 1, 0,13, 0,22, 0, 0, 0, 0, 0,11, 8,21,16, 0, 0,25, 0, 0,12,17],
              [ 0,17,25, 0,23, 7,14, 0,21, 1, 0, 0, 0, 0, 3, 0, 0,11, 0, 0,24, 0,16, 4, 5],
              [ 0, 0, 0, 0,11,18,24, 0, 0, 0, 0, 5, 0,12, 0,25, 0, 0, 0,15,23, 4, 8,14, 0],
              [ 0, 0, 0,15,21, 0, 0, 0, 0, 0, 2, 0,13,17, 0, 0, 1, 7, 0, 0, 5, 9,24, 0, 0],
              [ 0, 0,18, 0,22,15, 0, 0, 2,16, 0,23, 0, 0, 0,10, 6,24, 0,17,12, 0,25,11, 0],
              [ 7, 2, 0, 1, 0, 0,21, 0, 0, 0,18,22, 0, 9, 6,14, 0, 4, 5,16, 0, 0, 0, 0, 0],
              [ 0, 0, 9, 0, 0, 0, 7,22, 0, 0,10, 0,24, 0, 0, 0,18, 0, 0, 0,21, 0, 0, 0, 0],
              [ 0,12, 0,19,10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,14, 0, 4, 8, 0],
              [24, 0,11,18, 0, 0, 0, 0, 0, 0, 0,25,17,21, 0, 6, 0, 0, 1, 0, 0, 0, 0, 5,12],
              [16, 6,22, 0, 0, 0,23, 4,15,18, 8, 0, 0, 0,20, 0, 0,17, 0,14, 0, 0, 0, 0, 0],
              [ 0,21, 0, 0, 4, 0, 9, 1, 7, 0, 0, 0, 0,11,14, 0,16, 8,15, 0,22, 0,18, 0, 0],
              [ 8,15, 0, 0, 0, 0, 0, 0, 5, 0,24, 3, 0, 0, 4, 0, 0, 0, 9, 0, 0, 0, 0, 0,20]]



problems = {
    "world_hardest": {
       "problem": world_hardest,
       "size": 3
    },
    "another" : {
       "problem": another,
       "size" : 3
    },
    "problem_34" : {
       "problem" : problem_34,
       "size" : 4
    },
    "problem_89" : {
      "problem" : problem_89,
      "size": 5
    }
    }

prove_unicity = False
for p in problems:
    problem = problems[p]["problem"]
    size = problems[p]["size"]
    print(f"problem: {p} {size**2} x {size**2}")
    t0 = time.time()
    sudoku(problem, size, prove_unicity)
    t1 = time.time()
    print("time:", t1-t0,"\n")

