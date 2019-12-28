#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Heterosquare problem in Z3
#
# From http://willow.engr.uconn.edu/cometPubWiki/index.php/Heterosquare
# """
# A heterosquare of order n is a n*n square whose elements are distinct integers from 
# 1 to n^2 such that the sums of the rows, columns and diagonals are all different. 
# Here is an example of heterosquare of order 3 
# 
#            19
# 
# 1  2  3    6
# 8  9  4    21
# 7  6  5    18
# 
# 16 17 12   15  (Sums)
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

n = 3

#
# symmetry breaking
#
# From http://en.wikipedia.org/wiki/Heterosquare
# """
# It is strongly suspected that there are exactly 3120 
# essentially different heterosquares of order 3.
# """
#
# From
# http://en.wikipedia.org/wiki/Fr#C3#A9nicle_standard_form
# """
# A magic square is in Frénicle standard form, named for 
# Bernard Frénicle de Bessy, if the following two conditions apply:
#  - the element at position [1,1] (top left corner) is the smallest 
#    of the four corner elements; and
#  - the element at position [1,2] (top edge, second from left) is 
#    smaller than the element in [2,1].
# """
#
# Note: For n = 3 this gives 3120 solutions, as suspected...
#
def frenicle(x,n):
  minimum(sol,x[(0,0)], [x[(0,0)], x[(0,n-1)], x[(n-1,0)], x[(n-1,n-1)]])
  sol.add(x[(0,1)] < x[(1,0)])

# variables
x = {}
for i in range(n):
  for j in range(n):
    x[(i, j)] = makeIntVar(sol, "x(%i,%i)" % (i, j),1,n*n)

row_sums = makeIntVector(sol, "row_sums", n, 1,n*n*n)
col_sums = makeIntVector(sol, "col_sums", n, 1,n*n*n)

diag1 = makeIntVar(sol, "diag1",1,n*n*n)
diag2 = makeIntVar(sol, "diag2",1,n*n*n)

# constraints

# all the entries in the matrix should be different
sol.add(Distinct([x[(i,j)] for i in range(n) for j in range(n)]))

# and all sums should be different
sol.add(Distinct(row_sums + col_sums + [diag1, diag2]))

# calculate rows sums
for i in range(n):
  sol.add(row_sums[i] == Sum([x[(i,j)] for j in range(n)]))

# calculate column sums
for j in range(n):
  sol.add(col_sums[j] == Sum([x[(i,j)] for i in range(n)]))

# diag1 sums
sol.add(Sum([x[(i,i)] for i in range(n)]) == diag1)

# diag2 sums
sol.add(Sum([x[(i,n-i-1)] for i in range(n)]) == diag2)

# symmetry breaking (see above)
frenicle(x,n)

num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print("diag1:", mod.eval(diag1), "diag2:", mod.eval(diag2))
  print("row_sums:", [mod.eval(row_sums[i]) for i in range(n)])
  print("col_sums:", [mod.eval(col_sums[i]) for i in range(n)])
  for i in range(n):
    for j in range(n):
      print("%3i" % mod.eval(x[(i,j)]).as_long(),end=" ")
    print()
  print()
  getDifferentSolutionMatrix(sol,mod,x,n,n)

print("num_solutions:", num_solutions)




