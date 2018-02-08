#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Organizing a day in Z3
#
# Simple scheduling problem.
#
# Problem formulation from ECLiPSe:
# Slides on (Finite Domain) Constraint Logic Programming, page 38f
# http://eclipse-clp.org/reports/eclipse.ppt
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
from z3_utils_hakank import *

def main():
  # set_param(proof=True)
  
  sol = Solver()

  # data
  n = 4

  tasks = list(range(n))
  work, mail, shop, bank = tasks
  durations = [4, 1, 2, 1]

  # task [i,0] must be finished before task [i,1]
  before_tasks = [
      [bank, shop],
      [mail, work]
  ]

  # the valid times of the day
  begin = 9
  end = 17

  #
  # declare variables
  #
  begins = [makeIntVar(sol, "begins[%i]" % i, begin, end) for i in tasks]
  ends = [makeIntVar(sol, "ends[%i]" % i, begin, end) for i in tasks]

  # constraints
  for i in tasks:
    sol.add(ends[i] == begins[i] + durations[i])

  for i in tasks:
    for j in tasks:
      if i < j:
        no_overlap(sol,
                   begins[i], durations[i],
                   begins[j], durations[j])

  # specific constraints
  for (before, after) in before_tasks:
    sol.add(ends[before] <= begins[after])

  sol.add(begins[work] >= 11)

  # solution and search
  num_solutions = 0
  # print("sol:", sol)
  
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print('begins:', [mod.eval(begins[i]) for i in tasks])
    print('ends:', [mod.eval(ends[i]) for i in tasks])
    print()
    getDifferentSolution(sol,mod,begins,ends)

  print('num_solutions:', num_solutions)


if __name__ == '__main__':
  main()
