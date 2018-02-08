#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Coins grid problem in Z3
#
# Problem from
# Tony Hurlimann: "A coin puzzle - SVOR-contest 2007"
# http://www.svor.ch/competitions/competition2007/AsroContestSolution.pdf
# '''
# In a quadratic grid (or a larger chessboard) with 31x31 cells, one should
# place coins in such a way that the following conditions are fulfilled:
#    1. In each row exactly 14 coins must be placed.
#    2. In each column exactly 14 coins must be placed.
#    3. The sum of the quadratic horizontal distance from the main diagonal
#       of all cells containing a coin must be as small as possible.
#    4. In each cell at most one coin can be placed.
# The description says to place 14x31 = 434 coins on the chessboard each row
# containing 14 coins and each column also containing 14 coins.
# '''
#
# Original problem is:
#    n = 7 # 31 # The grid size
#    c = 4 # 14 # Number of coins per row/column
# which a traditional MIP solver solves in millis.
#
#
# Note that using Optimize() is much slower than using Solver():
# for n=7, c=4:
#   Optimize takes 13.5s
#   Solver takes 0.45s!
#
# Tested with SolverFor("LIA") and it's faster:
#  7,4: 0.45s (about the same as for Solver())
# 10,6: 0.69s
# 15,10: 3.1s
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
# 
from z3_utils_hakank import *

# Using Optimize(): slow
def coins_grid_optimize(n=7, c= 4):
    # set_option('smt.arith.solver', 3)
    # set_option("smt.arith.euclidean_solver",True) 

    sol = Optimize()
    # sol = OptimizeFor("LIA")
    # n = 7 # 31 # The grid size
    # c = 4 # 14 # Number of coins per row/column

    x = {}
    for i in range(n):
        for j in range(n):
            x[(i,j)] = Int("x[%i,%i]" % (i,j))
            sol.add(x[(i,j)] >= 0)
            sol.add(x[(i,j)] <= 1)

    # rows and columns == c
    for i in range(n):
        sol.add(c == Sum([x[(i,j)] for j in range(n)]))
        sol.add(c == Sum([x[(j,i)] for j in range(n)]))

    z = Int("z")

    # quadratic horizonal distance var
    sol.add(z == Sum([x[(i, j)] * (i - j) * (i - j)  for i in range(n) for j in range(n)]))

    sol.minimize(z)

    if sol.check() == sat:
        mod = sol.model()
        print "diff=",mod.evaluate(z)
        for i in range(n):
            for j in range(n):
                print mod.evaluate(x[(i, j)]),
            print
        print

    else:
        print "No solution"

#
# Using Solver() and handling the optimization step is _much_ faster.
#
def coins_grid_solver(n=7,c=4):
    # sol = Solver()
    sol = SolverFor("LIA") # This is much faster still,
    
    x = {}
    for i in range(n):
        for j in range(n):
            x[(i,j)] = Int("x[%i,%i]" % (i,j))
            sol.add(x[(i,j)] >= 0)
            sol.add(x[(i,j)] <= 1)

    # rows and columns == c
    for i in range(n):
        sol.add(c == Sum([x[(i,j)] for j in range(n)]))
        sol.add(c == Sum([x[(j,i)] for j in range(n)]))

    z = Int("z")

    # quadratic horizonal distance var
    sol.add(z == Sum([x[(i, j)] * (i - j) * (i - j)  for i in range(n) for j in range(n)]))

    while sol.check() == sat:
        mod = sol.model()
        print "diff=",mod.evaluate(z)
        for i in range(n):
            for j in range(n):
                print mod.evaluate(x[(i, j)]),
            print
        print
        getLessSolution(sol,mod,z)


coins_grid_optimize(7,4)
# coins_grid_solver(7,4)
# coins_grid_solver(31,14) # still too slow
# coins_grid_solver(10,6)
# coins_grid_solver(15,10)
