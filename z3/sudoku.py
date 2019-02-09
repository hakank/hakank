#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Sudoku in Z3
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
# 
from z3_utils_hakank import *

def sudoku(init,prove_unicity=True):
    # sol = Solver()
    # sol = SolverFor("QF_LIA")
    sol = SolverFor("LIA")  # faster
    m = 3
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
    # - Prove unicity: 0.725
    # - Just a solution: 0.503
    if prove_unicity:
        while sol.check() == sat:
            mod = sol.model()
            print_grid(mod,x,n,n)
            print
            getDifferentSolutionMatrix(sol,mod,x, n,n)
    else:
        if sol.check() == sat:        
            mod = sol.model()
            print_grid(mod,x,n,n)

# Problem from
# "World's hardest sudoku: can you crack it?"
# http://www.telegraph.co.uk/science/science-news/9359579/Worlds-hardest-sudoku-can-you-crack-it.html
#
# Tims to first solution: 0.37s
# Time to ensure unicity: 0.54s
# Note: Null time (i.e. just reading this model w/o any goal): 0.204s
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
# This model: 0,27s (0.31s to prove uniciy)
another = [[0,0,0,0,9,4,0,3,0],
           [0,0,0,5,1,0,0,0,7],
           [0,8,9,0,0,0,0,4,0],
           [0,0,0,0,0,0,2,0,8],
           [0,6,0,2,0,1,0,5,0],
           [1,0,2,0,0,0,0,0,0],
           [0,7,0,0,0,0,5,2,0],
           [9,0,0,0,6,5,0,0,0],
           [0,4,0,9,7,0,0,0,0]]

# sudoku(world_hardest, False)
# print
sudoku(another,True)


