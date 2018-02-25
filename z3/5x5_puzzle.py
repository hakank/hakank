#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# 5x5 puzzle (Five puzzle, Martin Chlond) in Z3
#
# From http://www.chlond.demon.co.uk/Five.html (Java applet).
# (Link from http://www.chlond.demon.co.uk/puzzles/puzzles1.html)
# 
# """
# Each of the squares in the above grid can be in one of two states, lit(white)
# or unlit(red). If the player clicks on a square then that square and each 
# orthogonal neighbour will toggle between the two states. Each mouse click 
# constitutes one move and the objective of the puzzle is to light all 
# 25 squares in the least number of moves.
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = SolverFor("LIA")

n = 5

# variables
x = makeIntVectorMatrix(sol, "x", n,n, 0,1)
d = makeIntVectorMatrix(sol, "d", n,n, 0,n)
the_sum = Int("the_sum")

# constraints
sol.add(the_sum == Sum([x[(i,j)] for i in range(n) for j in range(n)]))

for i in range(n):
    for j in range(n):
        sol.add(2*d[(i,j)]+1
                ==
                Sum([x[(i,k)] for k in range(j-1,j+2) if k >= 0 and k < n and k != j])
                + 
                Sum([x[(k,j)] for k in range(i-1,i+2) if k >= 0 and k < n])
                )


num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print "the_sum:", mod[the_sum]
  print "x:\n",
  for i in range(n):
      for j in range(n):
          print mod[x[(i,j)]],
      print
  print
  getDifferentSolutionMatrix(sol,mod,x,n,n)

print "num_solutions:", num_solutions
