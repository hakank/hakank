#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Stable marriage problem in Z3
#
# http://mathworld.wolfram.com/StableMarriageProblem.html
# """
# Given a set of n men and n women, marry them off in pairs after each man has ranked the
# women in order of preference from 1 to n, {w_1,...,w_n} and each women has done likewise,
# {m_1,...,m_n}. If the resulting set of marriages contains no pairs of the form
# {m_i,w_j}, {m_k,w_l} such that m_i prefers w_l to w_j and w_l prefers m_i to m_k,
# the marriage is said to be stable. Gale and Shapley (1962) showed that a stable marriage exists
# for any choice of rankings (Skiena 1990, p. 245).
# In the United States, the algorithm of Gale and Shapley (1962) is used to match hospitals to
# medical interns (Skiena 1990, p. 245). 
# """
# 
# Translation of the OPL version (via MiniZinc) from
# Pascal Van Hentenryck "The OPL Optimization Programming Language"
# E.g.
# http://www.comp.rgu.ac.uk/staff/ha/ZCSP/additional_problems/stable_marriage/stable_marriage.pdf
#
#
# Note: Here Array() is used to be able to handle (matrix) element constraints.
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from __future__ import print_function
import sys
from z3_utils_hakank import *

def main(ranks, problem_name):

  # sol = Solver() # 0.938s
  # sol = SolverFor("QF_FD") # Don't work
  sol = SimpleSolver() # 0.787s
  # sol = SolverFor("LIA") # 0.865s
  # sol = SolverFor("QF_LIA") # 1.387s
  # sol = SolverFor("NIA") # 0.935s
  # sol = SolverFor("QF_NIA") # 0.855s
  
  
  # data
  print("Problem name:", problem_name)

  rankMen = ranks["rankMen"]
  rankWomen = ranks["rankWomen"]

  n = len(rankMen)

  #
  # declare variables
  #
  # wife = [Int("wife[%i]" % i) for i in range(n)] # don't work with element/3
  # husband = [Int("husband[%i]" % i) for i in range(n)] # don't work with element/3

  rankMen_a = copyArrayMatrix(sol,rankMen,"rankMen_a",n,n,1,n)
  rankWomen_a = copyArrayMatrix(sol,rankWomen,"rankWomen_a",n,n,1,n)  

  # NOTE: Array don't support matrix_element. TO BE INVESTIGATED FURTHER!
  # wife = Array("wife",IntSort(),IntSort())
  # husband = Array("husband",IntSort(),IntSort())
  # for i in range(n):
  #     sol.add(wife[i]>= 0,wife[i] <= n-1)
  #     sol.add(husband[i]>= 0,husband[i] <= n-1)      
  wife = makeIntArray(sol,"wife", n,0,n-1)
  husband = makeIntArray(sol,"husband", n,0,n-1)  


  #
  # constraints
  #

  # forall(m in Men)
  #    cp.post(husband[wife[m]] == m);
  for m in range(n):
    sol.add(husband[wife[m]] == m)

  # forall(w in Women)
  #    cp.post(wife[husband[w]] == w);
  for w in range(n):
    sol.add(wife[husband[w]] == w)

  # forall(m in Men, o in Women)
  # cp.post(rankMen[m,o] < rankMen[m, wife[m]] => rankWomen[o,husband[o]] <
  # rankWomen[o,m]);
  for m in range(n):
    for o in range(n):
        # sol.add(If(rankMen[m][o] < rankMen[m][wife[m]], rankWomen[o][husband[o]] < rankWomen[o][m]))
        sol.add(Implies(rankMen_a[m*n+o] < rankMen_a[m*n+wife[m]], rankWomen_a[o*n+husband[o]] < rankWomen_a[o*n+m]))

  # forall(w in Women, o in Men)
  # cp.post(rankWomen[w,o] < rankWomen[w,husband[w]] => rankMen[o,wife[o]] < rankMen[o,w]);
  for w in range(n):
    for o in range(n):
      sol.add(Implies(rankWomen_a[w*n+o] < rankWomen_a[w*n+husband[w]], rankMen_a[o*n+wife[o]] < rankMen_a[o*n+w]))

  #
  # solution and search
  #
  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print("wife   : ", [mod.eval(wife[i]) for i in range(n)])
    print("husband: ", [mod.eval(husband[i]) for i in range(n)])
    print()
    getDifferentSolution(sol,mod,[wife[i] for i in range(n)],[husband[i] for i in range(n)])
    
  print("num_solutions:", num_solutions)
  print()


#
# From Van Hentenryck's OPL book
#
van_hentenryck = {
    "rankWomen": [
        [1, 2, 4, 3, 5],
        [3, 5, 1, 2, 4],
        [5, 4, 2, 1, 3],
        [1, 3, 5, 4, 2],
        [4, 2, 3, 5, 1]
    ],

    "rankMen": [
        [5, 1, 2, 4, 3],
        [4, 1, 3, 2, 5],
        [5, 3, 2, 4, 1],
        [1, 5, 4, 3, 2],
        [4, 3, 2, 1, 5]
    ]
}

#
# Data from MathWorld
# http://mathworld.wolfram.com/StableMarriageProblem.html
#
mathworld = {
    "rankWomen": [
        [3, 1, 5, 2, 8, 7, 6, 9, 4],
        [9, 4, 8, 1, 7, 6, 3, 2, 5],
        [3, 1, 8, 9, 5, 4, 2, 6, 7],
        [8, 7, 5, 3, 2, 6, 4, 9, 1],
        [6, 9, 2, 5, 1, 4, 7, 3, 8],
        [2, 4, 5, 1, 6, 8, 3, 9, 7],
        [9, 3, 8, 2, 7, 5, 4, 6, 1],
        [6, 3, 2, 1, 8, 4, 5, 9, 7],
        [8, 2, 6, 4, 9, 1, 3, 7, 5]],

    "rankMen": [
        [7, 3, 8, 9, 6, 4, 2, 1, 5],
        [5, 4, 8, 3, 1, 2, 6, 7, 9],
        [4, 8, 3, 9, 7, 5, 6, 1, 2],
        [9, 7, 4, 2, 5, 8, 3, 1, 6],
        [2, 6, 4, 9, 8, 7, 5, 1, 3],
        [2, 7, 8, 6, 5, 3, 4, 1, 9],
        [1, 6, 2, 3, 8, 5, 4, 9, 7],
        [5, 6, 9, 1, 2, 8, 4, 3, 7],
        [6, 1, 4, 7, 5, 8, 3, 9, 2]]
}

#
# Data from
# http://www.csee.wvu.edu/~ksmani/courses/fa01/random/lecnotes/lecture5.pdf
#
problem3 = {
    "rankWomen": [
        [1, 2, 3, 4],
        [4, 3, 2, 1],
        [1, 2, 3, 4],
        [3, 4, 1, 2]],

    "rankMen": [
        [1, 2, 3, 4],
        [2, 1, 3, 4],
        [1, 4, 3, 2],
        [4, 3, 1, 2]]
}


#
# Data from
# http://www.comp.rgu.ac.uk/staff/ha/ZCSP/additional_problems/stable_marriage/stable_marriage.pdf
# page 4
#
problem4 = {
    "rankWomen": [
        [1, 5, 4, 6, 2, 3],
        [4, 1, 5, 2, 6, 3],
        [6, 4, 2, 1, 5, 3],
        [1, 5, 2, 4, 3, 6],
        [4, 2, 1, 5, 6, 3],
        [2, 6, 3, 5, 1, 4]],

    "rankMen": [
        [1, 4, 2, 5, 6, 3],
        [3, 4, 6, 1, 5, 2],
        [1, 6, 4, 2, 3, 5],
        [6, 5, 3, 4, 2, 1],
        [3, 1, 2, 4, 5, 6],
        [2, 3, 1, 6, 5, 4]]
}


if __name__ == "__main__":
  main(van_hentenryck, "Van Hentenryck")
  main(mathworld, "MathWorld")
  main(problem3, "Problem 3")
  main(problem4, "Problem4")
