#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Langford's number problem  in Z3
# 
#   Langford's number problem (CSP lib problem 24)
#   http://www.csplib.org/prob/prob024/
#   '''
#   Arrange 2 sets of positive integers 1..k to a sequence,
#   such that, following the first occurence of an integer i,
#   each subsequent occurrence of i, appears i+1 indices later
#   than the last.
#   For example, for k=4, a solution would be 41312432
#   '''
#
#   * John E. Miller: Langford's Problem
#     http://www.lclark.edu/~miller/langford.html
#
#   * Encyclopedia of Integer Sequences for the number of solutions for each k
#     http://www.research.att.com/cgi-bin/access.cgi/as/njas/sequences/eisA.cgi?Anum=014552
#
#
# This version use IntVector and the explicit element constraint. It's slightly faster
# than langford.py (which use IntArray and the "built-in" element constraint).
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
# 
from z3_utils_hakank import *


def langford(k=8, num_sol=0):

    sol = Solver()

    # data
    print "k:", k
    p = list(range(2 * k))

    #
    # declare variables
    #
    position = makeIntVector(sol, "position", 2*k, 0, 2*k-1) 
    solution = makeIntVector(sol, "solution", 2*k, 1, k)

    # constraints
    sol.add(Distinct([position[i] for i in p ]))

    # aux constraints: solution count 1..k == 2 (much slower)
    # for i in range(1,k+1):
    #     sol.add(2 == Sum([If(solution[j] == i,1,0) for j in range(k*2)] ))

    for i in range(1, k+1):
        sol.add(position[i+k-1] == position[i-1]+i+1)
        element(sol,position[i-1],solution,i,2*k)
        element(sol,position[k+i-1],solution,i,2*k)
        # sol.add(solution[position[i-1]] == i)
        # sol.add(solution[position[k+i-1]] ==i)

    # symmetry breaking
    sol.add(solution[0] < solution[2 * k - 1])

    # search and result
    num_solutions = 0
    while sol.check() == sat:
        num_solutions += 1
        mod = sol.model()
        # print "position:", [mod.eval(position[i]) for i in p]
        print "solution:", [mod.eval(solution[i]) for i in p]
        getDifferentSolution(sol,mod,solution)
        if num_sol > 0 and num_solutions >= num_sol:
            break
    print "num_solutions:", num_solutions

k = 8
num_sol = 0
if __name__ == "__main__":
  if len(sys.argv) > 1:
    k = int(sys.argv[1])
  if len(sys.argv) > 2:
    num_sol = int(sys.argv[2])

  langford(k, num_sol)


