#
# Latin square with diagonals in z3.
# Inspired by  Eric Taucher's post
# "Latin squares" in the SWI-Prolog forum
# https://swi-prolog.discourse.group/t/latin-squares/5056
#
# http://en.wikipedia.org/wiki/Latin_square:
# """
# A Latin square is an n X n table filled with n different symbols in
# such a way that each symbol occurs exactly once in each row and
# exactly once in each column. 
# """
#
# This variant also includes the constraint that the two diagonals must
# be distinct.
#
#  Also see: https://oeis.org/A274806
#  Here are the counts from 1..8:
#  1, 0, 0, 48, 960, 92160, 862848000, 300286741708800
#
# For solver QD_LIA:
#   n  #sols  time
#   --------------------
#   1     1     0.008s
#   2     0     0.003s
#   3     0     0.004s
#   4    48     0.114s
#   5   960     3.259s
#   6 92160  1003.95s
#
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
import time
from z3 import *

def latin_square(x):
    """
    Ensure that the matrix x is a Latin square
    """
    n = len(x)
    constraints = []
    for i in range(n):
        constraints += [Distinct([x[i][j] for j in range(n)])]
        constraints += [Distinct([x[j][i] for j in range(n)])]
    return constraints

def latin_squares(n=4,print_sol=True):
    """
    Find (and show if print_sol=True) all Latin squares problem of size n.
    """

    # s = Solver()
    # s = SimpleSolver()
    s = SolverFor("QF_LIA")
    # s = SolverFor("QF_NIA")
    # s = SolverFor("ALL")
    # s = SolverFor("LIA")
    
    x = [ [Int(f"x[{i},{j}])" ) for j in range(n)] for i in range(n)]

    for i in range(n):
        for j in range(n):
            s.add(x[i][j] >= 1, x[i][j] <= n)
            
    s.add(latin_square(x))

    num_solutions = 0
    while s.check() == sat:
        num_solutions += 1
        m = s.model()
        if print_sol:
            for i in range(n):
                for j in range(n):
                    print(m[x[i][j]], end = " ")
                print()
            print()
        s.add(Or([ x[i][j] != m[x[i][j]] for i in range(n) for j in range(n)  ]))
    print("num_solutions:", num_solutions)


def latin_squares_with_diagonals(n=4, print_sol=True):
    """
    Find (and show if print_sol=True) all Latin square with digonals of size n.
    """
    
    # s = Solver() 
    # s = SimpleSolver() 
    s = SolverFor("QF_LIA")
    # s = SolverFor("LIA")
    
    x = [ [Int(f"x[{i},{j}])" ) for j in range(n)] for i in range(n)]

    for i in range(n):
        for j in range(n):
            s.add(x[i][j] >= 1, x[i][j] <= n)

    # It's a Latin square
    s.add(latin_square(x))

    # The diagonal constraints
    s.add(Distinct([x[i][i] for i in range(n)]) )  # diag 1
    s.add(Distinct([x[i][n-i-1] for i in range(n)]))  # diag 2

    num_solutions = 0
    while s.check() == sat:
        num_solutions += 1
        m = s.model()
        if print_sol:
            for i in range(n):
                for j in range(n):
                    print(m[x[i][j]], end = " ")
                print()
            print()
        s.add(Or([ x[i][j] != m[x[i][j]] for i in range(n) for j in range(n)  ]))
    print("num_solutions:", num_solutions)

for n in range(1,6+1):
    print("n:", n)
    t0 = time.time()
    latin_squares_with_diagonals(n,False)
    t1 = time.time()
    print("Time: ", t1-t0)
    print()
