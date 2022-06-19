"""
Diamond-free Degree Sequences in cpmpy.

This is a port of the Numberjack example DiamondfreeDegreeSequences.py:
'''
Fill in a binary matrix of size n * n in such a way that
- For every grouping of four rows, the sum of their non-symmetrical
  values is less than or equal to 4,
- No rows contain just zeroes,
- Every row has a sum modulo 3,
- The sum of the matrix is modulo 12.
- No row R contains a 1 in its Rth column.

Note on first constraint in model:
A group of four nodes can have at most four edges between them.
Since the matrix for this model will represent the adjacency
matrix of the graph, we need to take into consideration the fact
that the matrix will be symmetrical.

CSPLib Problem 050 - http://www.csplib.org/Problems/prob050/
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *
from itertools import combinations



def get_model(N):
    # By definition a and b will have the same cardinality:
    matrix = boolvar(shape=(N, N), name="matrix")

    model = Model(
        # No rows contain just zeroes.
        [sum(row) > 0 for row in matrix],

        # Every row has a sum modulo 3.
        [sum(row) % 3 == 0 for row in matrix],

        # The sum of the matrix is modulo 12.
        sum(matrix) % 12 == 0,

        # No row R contains a 1 in its Rth column.
        [matrix[row][row] == 0 for row in range(N)])

    # Every grouping of 4 rows can have at most a sum of 4 between them.
    for a, b, c, d in combinations(range(N), 4):
        model += sum([matrix[a][b], matrix[a][c], matrix[a][d],
                      matrix[b][c], matrix[b][d], matrix[c][d]]) <= 4

    # Undirected graph
    for i in range(N):
        model += matrix[i][i] == 0  # No looping arcs

        for j in range(N):
            model += matrix[i][j] == matrix[j][i]
    
    # Symmetry breaking
    model += lex2(matrix)

    return matrix, model


def solve(param):
    N = param['N']

    matrix, model = get_model(N)

    def print_sol():
        print(matrix.value())
        print("Degree sequence:",end= " ")
        for row in matrix:
            print(sum([x.value() for x in row]),end=" ")
        print()
        print()

    ss = CPM_ortools(model)
    num_solutions = ss.solveAll(display=print_sol)
    print("num_solutions:",num_solutions)

default = {'N': 10}
solve(default)

