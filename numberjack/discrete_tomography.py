#!/usr/bin/python
"""
Discrete tomography in Numberjack.

Problem from http://eclipse.crosscoreop.com/examples/tomo.ecl.txt
'''
This is a little "tomography" problem, taken from an old issue
of Scientific American.

A matrix which contains zeroes and ones gets "x-rayed" vertically and
horizontally, giving the total number of ones in each row and column.
The problem is to reconstruct the contents of the matrix from this
information. Sample run:

?- go.
    0 0 7 1 6 3 4 5 2 7 0 0
 0                         
 0                         
 8      * * * * * * * *    
 2      *             *    
 6      *   * * * *   *    
 4      *   *     *   *    
 5      *   *   * *   *    
 3      *   *         *    
 7      *   * * * * * *    
 0                         
 0                         


Eclipse solution by Joachim Schimpf, IC-Parc
'''

Compare with the following models: 
* Comet: http://www.hakank.org/comet/discrete_tomography.co
* Gecode: http://www.hakank.org/gecode/discrete_tomography.cpp
* MiniZinc: http://www.hakank.org/minizinc/tomography.mzn
* Tailor/Essence': http://www.hakank.org/tailor/tomography.eprime

This model was created by
Hakan Kjellerstrand (hakank@bonetmail.com)

See also my Numberjack page http://www.hakank.org/numberjack/

"""
import sys
from Numberjack import *

#
# Note: Mistral has problem with this for
# Numberjack version 0.11
#
def discrete_tomography(libs, row_sums="", col_sums=""):

    if row_sums == "":
        print "Using default"
        row_sums = [0,0,8,2,6,4,5,3,7,0,0]
        col_sums = [0,0,7,1,6,3,4,5,2,7,0,0]


    r = len(row_sums)
    c = len(col_sums)

    x = Matrix(r, c, 0, 1)

    model = Model([
        [Sum(row) == row_sums[i] for (row,i) in zip(x.row, range(r))],
        [Sum(col) == col_sums[j] for (col,j) in zip(x.col, range(c))]
        ]
        )

    for library in libs:
        solver = model.load(library)
        
        print 'library:', library
        if solver.solve():
            solver.printStatistics()
            print_board(x, r, c,row_sums, col_sums)
            num_solutions = 1
            print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
            while library == 'Mistral' and solver.getNextSolution():
                print_board(x, r, c, row_sums, col_sums)
                print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime(), " Failures: ", solver.getFailures()
                print ''
                num_solutions += 1
            print "number of solutions:", num_solutions
            print 'Nodes:', solver.getNodes(), ' Time:', solver.getTime()
        else:
            print "No solution"
        print ''

#
# Read a problem instance from a file
#
def read_problem(file):
    f = open(file, 'r')
    row_sums = f.readline()
    col_sums = f.readline()
    row_sums = [int(r) for r in (row_sums.rstrip()).split(",")]
    col_sums = [int(c) for c in (col_sums.rstrip()).split(",")]
    
    return [row_sums, col_sums]


#
# Print the mines
#
def print_board(x, rows, cols, row_sums, col_sums):
    print "  ",
    for j in range(cols):
        print col_sums[j],
    print 
    for i in range(rows):
        for j in range(cols):
            if x[i][j].get_value() == 1:
                print "#",
            else:
                print ".",
        print ''

if len(sys.argv) > 1:
    file = sys.argv[1]
    print "Problem instance from", file
    [row_sums, col_sums] = read_problem(file)
    discrete_tomography(['Mistral'], row_sums, col_sums)    
    # discrete_tomography(['Mistral','SCIP'], row_sums, col_sums)
else:    

    discrete_tomography(['Mistral'])
    # discrete_tomography(['SCIP'])        
