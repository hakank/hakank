#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# nqueen problem in Z3
#
# This is a port of Neng-Fa Zhou's Picat (SAT) model.
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

#
# N-queen (matrix form)
#
def nqueen(n=8,m=0):

    sol = SolverFor("QF_LIA")
    # sol = Solver()
    
    x = {}
    for i in range(n):
      for j in range(n):
        x[(i,j)] = makeIntVar(sol,"x[%i,%i]"%(i,j), 0,1)

    for i in range(n): # 1 in each row
       sol.add(Sum([x[(i,j)] for j in range(n)]) == 1)

    for j in range(n): # 1 in each column
       sol.add(Sum([x[(i,j)] for i in range(n)]) == 1)

    for k in range(1-n,n-1): # at most one
       sol.add(Sum([x[(i,j)] for i in range(n) for j in range(n) if i-j== k]) <= 1)

    for k in range(1,2*n):   # at most one
       sol.add(Sum([x[(i,j)] for i in range(n) for j in range(n) if i+j == k]) <= 1)

    num_solutions = 0
    while sol.check() == sat:
      num_solutions += 1
      mod = sol.model()
      # print_grid(mod, x,n,n)
      # present solution as an array instead
      for i in range(n):
        for j in range(n):
          if mod.eval(x[(i,j)]).as_long() == 1:
            print j+1,
      print
      if m == 0 or (m > 0 and num_solutions < m):
        getDifferentSolutionMatrix(sol,mod,x,n,n)
      else:
        break

    print
    print "num_solutions:", num_solutions


n = 8
m = 0
if __name__ == "__main__":
  if len(sys.argv) > 1:
    n = int(sys.argv[1])
  if len(sys.argv) > 1:
    m = int(sys.argv[2])
    
  nqueen(n,m)

