"""
Balanced Incomplete Block Design (BIBD) in cpmpy.

This is a port of Numberjack example Bibd.py:
'''
Balanced Incomplete Block Design (BIBD) --- CSPLib prob028

A BIBD is defined as an arrangement of v distinct objects into b blocks such
that each block contains exactly k distinct objects, each object occurs in
exactly r different blocks, and every two distinct objects occur together in
exactly lambda blocks. Another way of defining a BIBD is in terms of its
incidence matrix, which is a v by b binary matrix with exactly r ones per row,
k ones per column, and with a scalar product of lambda 'l' between any pair of
distinct rows.
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def get_model(v, b, r, k, l):
    matrix = boolvar(shape=(v, b),name="matrix")
    model = Model(
        [sum(row) == r for row in matrix],              # every row adds up to r
        [sum(col) == k for col in matrix.transpose()],  # every column adds up to k

        # the scalar product of every pair of columns adds up to l
        [sum([(row[col_i] * row[col_j]) for row in matrix]) == l
            for col_i in range(v) for col_j in range(col_i)],
    )
    
    return matrix, model


def solve(param):
    matrix, model = get_model(param['v'], param['b'], param['r'], param['k'], param['l'])
    model += lex2(matrix)

    ss = CPM_ortools(model)
    num_solutions = ss.solveAll(display=matrix)
    print("num_solutions:",num_solutions)


default = {'v': 7, 'b': 7, 'r': 3, 'k': 3, 'l': 1, 'num_sols':0}
print(solve(default))
