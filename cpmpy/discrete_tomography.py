"""
Discrete tomography in cpmpy.

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

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def discrete_tomography(row_sums="", col_sums=""):

    if row_sums == "":
        print("Using default problem")
        row_sums = [0,0,8,2,6,4,5,3,7,0,0]
        col_sums = [0,0,7,1,6,3,4,5,2,7,0,0]


    r = len(row_sums)
    c = len(col_sums)

    x = intvar(0,1,shape=(r, c), name="x")

    model = Model([
        [sum(row) == row_sums[i] for (row,i) in zip(x, range(r))],
        [sum(col) == col_sums[j] for (col,j) in zip(x.transpose(), range(c))]
        ]
        )

    def print_sol():
        print_board(x, r, c, row_sums, col_sums)

    ss = CPM_ortools(model)
    num_solutions = ss.solveAll(display=print_sol)    
    print("number of solutions:", num_solutions)

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
    # print("",end=" ")
    for j in range(cols):
        print(col_sums[j],end=" ")
    print()
    for i in range(rows):
        for j in range(cols):
            if x[i][j].value() == 1:
                print("#",end=" ")
            else:
                print(".",end=" ")
        print()

if len(sys.argv) > 1:
    file = sys.argv[1]
    print("Problem instance from", file)
    [row_sums, col_sums] = read_problem(file)
    discrete_tomography(row_sums, col_sums)    
else:
    discrete_tomography()
