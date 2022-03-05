#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Golomb's ruler problem in Z3
#
# From http://www.dis.uniroma1.it/~tmancini/index.php?currItem=research.publications.webappendices.csplib2x.problemDetails&problemid=006
# """
# Problem description
# Given a positive integer 'n_marks', this problem amounts to put 'n_marks' marks on a ruler, in such a
# way that the n_marks(n_marks - 1)/2 distances among them are all different. The objective is to find
# the length of the shortest ruler that admits the positioning of 'n_marks' marks.
#
# Problem input
#
#     * n_marks, the requested number of marks to be positioned on the ruler
#     * maxval, an upper bound of the ruler length 
#
# Search space
# The set of all possible positionings of n_marks on the ruler, i.e., the set of all total
# functions from [1..n_marks] (marks) to [0..maxval] (positions)
#
# Objective function
# Minimize ruler length
#
# Constraints
#
#     * C1: First mark must be set to 0
#     * C2: Distances between different marks must be all different
#     * C3: Ruler mark values must be in ascending order
# """
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
# 
from z3_utils_hakank import *

def golomb(size=8):

  # sol = Optimize() # > 40s
  # sol = Solver() # 17.4s
  # sol = SimpleSolver() # 16.3s
  # sol = SolverFor("QF_LIA") # 13.5s
  sol = SolverFor("QF_FD") # 0.97s
  # sol = SolverFor("LIA") # 16.7s
  # sol = SolverFor("QF_NIA") # 13.5s

  var_max = size * size
  all_vars = list(range(0, size))

  marks = [Int('marks_%d' % i) for i in all_vars]
  for i in all_vars:
      sol.add(marks[i] >= 0, marks[i] <= var_max)

  sol.add(marks[0] == 0)
  increasing_strict(sol, marks)
  sol.add(Distinct([marks[j] - marks[i]
                    for i in range(0, size - 1)
                    for j in range(i + 1, size)]))

  sol.add(marks[size - 1] - marks[size - 2] > marks[1] - marks[0])

  # sol.minimize(marks[size - 1])

  while sol.check() == sat:
      mod = sol.model()
      print("marks:", [mod.eval(marks[i]) for i in all_vars])
      getLessSolution(sol,mod,marks[size-1])

n = 8
if __name__ == '__main__':
  golomb(n)
