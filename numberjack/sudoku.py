#!/usr/bin/python
"""
Sudoku problem in Numberjack.

This is a straightforward implementation of Sudoku.

For more about Sudoku see:
http://en.wikipedia.org/wiki/Sudoku

Compare with the MiniZinc model:
* http://www.hakank.org/minizinc/sudoku_gcc_problems/
(which use global cardinality constraint instead of alldifferent)


This Numberjack model was created by
Hakan Kjellerstrand (hakank@bonetmail.com)

See also my Numberjack page http://www.hakank.org/numberjack/
"""
from Numberjack import *
#from Mistral import Solver
#from SCIP import Solver

def sudoku(libs):

    #
    # This is problem 0 from
    # Gecode's sudoku.cpp
    # http://www.gecode.org/gecode-doc-latest/sudoku_8cpp-source.html
    #
    puzzle = [
        [0,  0,  0,  2,  0,  5,  0,  0,  0],
        [0,  9,  0,  0,  0,  0,  7,  3,  0],
        [0,  0,  2,  0,  0,  9,  0,  6,  0],
        [2,  0,  0,  0,  0,  0,  4,  0,  9],
        [0,  0,  0,  0,  7,  0,  0,  0,  0],
        [6,  0,  9,  0,  0,  0,  0,  0,  1],
        [0,  8,  0,  4,  0,  0,  1,  0,  0],
        [0,  6,  3,  0,  0,  0,  0,  8,  0],
        [0,  0,  0,  6,  0,  8,  0,  0,  0]
        ]

    n = 9
    x = Matrix(n,n, 1,9)

    model = Model()

    #
    # set the clues
    #
    for i in range(0,n):
        for j in range(0,n):
            if puzzle[i][j] > 0:
                model.add(x[i,j] == puzzle[i][j])

    #
    # rows and columns must be different
    #
    model.add(
        [AllDiff(row) for row in x.row],
        [AllDiff(col) for col in x.col]
        )

    #
    # the cells must be different
    #
    reg = 3
    for i in range(0,reg):
        for j in range(0,reg):
            model.add(AllDiff([x[r,c] for r in range(i*reg,i*reg+3) for c in range(j*reg,j*reg+3)]))

    for library in libs:
        solver = model.load(library) # Load up model into solver
        # solver.setHeuristic("Impact", "Lex")
        print ''
        solver.solve()
        solver.printStatistics()
        for i in range(0,n):
            print x.row[i]
        print ''
        print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
        if library in ("Mistral", "SCIP"):
            num_solutions = 1
            while solver.getNextSolution() == SAT:
                print x
                print
                num_solutions += 1
            print "number of solutions: ", num_solutions
            print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
                

# sudoku(('SCIP', 'Mistral'))
sudoku(['Mistral'])
