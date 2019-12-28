#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Talisman Square in Z3
# http://mathworld.wolfram.com/TalismanSquare.html
# """
# An n×n array  of the integers from 1 to n^2 such that the difference between 
# any one integer and its neighbor (horizontally, vertically, or diagonally, without 
# wrapping around) is greater than or equal to some value k is called a (n,k)-talisman 
# square. 
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

def talisman_square(n=5,k=2):
  sol = Solver()
  
  x = {}
  for i in range(n):
    for j in range(n):
      x[(i,j)] = makeIntVar(sol,"x[%i,%i]" %(i,j),1,n*n)

    
  sol.add(Distinct([x[(i,j)] for i in range(n) for j in range(n)]))

  for i in range(1,n):
    for j in range(1,n):
      sol.add(Abs(x[i,j]-x[i-1,j]) >= k)
      sol.add(Abs(x[i,j]-x[i,j-1]) >= k)

  for i in range(n-1):
    for j in range(n-1):
      sol.add(Abs(x[i,j]-x[i+1,j]) >= k)
      sol.add(Abs(x[i,j]-x[i,j+1]) >= k)


  # symmetry breaking
  # sol.add(x[(0,0)] == 1)

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    for i in range(n):
      for j in range(n):
        print("%3i" % mod.eval(x[(i,j)]).as_long(),end=" ")
      print()
    print()
    getDifferentSolutionMatrix(sol,mod,x,n,n)

  print("num_solutions:", num_solutions)

n = 5
k = 2
if __name__ == "__main__":
  if len(sys.argv) > 1:
    n = int(sys.argv[1])
  if len(sys.argv) > 2:
    k = int(sys.argv[2])
  talisman_square(n,k)
