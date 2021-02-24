#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Steiner triplets in Z3
#
# """
# http://www.probp.com/examples/clpset/steiner.pl 
# The ternary Steiner problem of order n is to find n(n-1)/6 sets of elements in {1,2,...,n} 
# such that each set contains three elements and any two sets have at most one element in common. 
# For example, the following shows a solution for size n=7:
#
#      {1,2,3}, {1,4,5}, {1,6,7}, {2,4,6}, {2,5,7}, {3,4,7}, {3,5,6}
#
# Problem taken from:
#  C. Gervet: Interval Propagation to Reason about Sets: Definition and Implementation of a Practical 
#  Language,  Constraints, An International Journal, vol.1, pp.191-246, 1997.
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *


# number of common elements in two "sets"
def union_card(sol,s1,s2,common):
  sol.add(common == Sum([If(ss1 + ss2 == 2,1,0) for ss1,ss2 in zip(s1,s2)]))


def steiner(n=7,num_sols=1):
  print("n:",n, "num_sols:", num_sols)
  # sol = Solver()
  sol = SolverFor("LIA")
  nb = int(n*(n-1) // 6)

  if not(n % 6 == 1 or n % 6 == 3):
    print("N must be (1|3) modulo 6")
    return()

  sets = {}
  for i in range(nb):
    for j in range(n):
      sets[(i,j)] = makeIntVar(sol,"sets[%i,%i]" % (i,j), 0, 1)
  
  # symmetry breaking
  sol.add(sets[(0,0)] == 1)

  for i in range(nb):
    s1 = [sets[(i,k)] for k in range(n)]
    sol.add(3 == Sum(s1))
    for j in range(nb):
      if i > j:
        # ensure that s1 and s2 has max 1 element in common
        s2 = [sets[(j,k)] for k in range(n)]
        sol.add(3 == Sum(s2))
        # ensure 0..1 in the variable
        common = makeIntVar(sol,"common[%i,%i]" % (i,j),0,1)
        union_card(sol,s1,s2,common)
        # sol.add(common <= 1)

  num_solutions = 0 
  while sol.check() == sat :
    num_solutions += 1
    mod = sol.model()
    for i in range(nb):
      print([j for j in range(n) if mod.eval(sets[(i,j)]).as_long() == 1], end=" ")
    print()
    if num_sols > 0 and num_solutions >= num_sols:
      break
    
    if num_solutions % 1000 == 0:
        print("num_solutions:", num_solutions)
    getDifferentSolutionMatrix(sol,mod,sets,nb,n)

  if num_sols != 1:
    print("num_solutions:", num_solutions  )
               

                
n = 7
if __name__ == "__main__":
  if len(sys.argv) > 1:
    n = int(sys.argv[1])
    steiner(n)
    print()
  else:
    for n in range(3,10):
      if n % 6 == 1 or n % 6 == 3:
        steiner(n,1)
        print()
