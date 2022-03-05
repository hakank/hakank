#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Moving furnitures (scheduling) problem in Z3
#
# Marriott & Stukey: 'Programming with constraints', page  112f
#
# The model use an experimental decomposition of the
# global constraint cumulative. See z3_utils_hakank.py
# for the definition.
#
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
# 
from __future__ import print_function
from z3_utils_hakank import *

#
# Decompositon of cumulative.
#
# Inspired by the MiniZinc implementation:
# http://www.g12.csse.unimelb.edu.au/wiki/doku.php?id=g12:zinc:lib:minizinc:std:cumulative.mzn&s[]=cumulative
# The MiniZinc decomposition is discussed in the paper:
# A. Schutt, T. Feydy, P.J. Stuckey, and M. G. Wallace.
# 'Why cumulative decomposition is not as bad as it sounds.'
# Download:
# http://www.cs.mu.oz.au/%7Epjs/rcpsp/papers/cp09-cu.pdf
# http://www.cs.mu.oz.au/%7Epjs/rcpsp/cumu_lazyfd.pdf
#
#
# Parameters:
#
# s: start_times    assumption: array of IntVar
# d: durations      assumption: array of int
# r: resources      assumption: array of int
# b: resource limit assumption: IntVar or int
#
# Note: since I don't know how to extract the bounds of the
#       domains, both times_min and times_max1 are required
#       which is the lower/upper limits of s (the start_times)
#
# def my_cumulative(sol, s, d, r, b,times_min,times_max1):

#   tasks = [i for i in range(len(s)) if r[i] > 0 and d[i] > 0]
  
#   # how do I get the upper/lower value of a decision variable?
#   # times_min = min([s[i].Min() for i in tasks])
#   # times_max = max([s[i].Max() + max(d) for i in tasks])
#   times_max = times_max1 + max(d)
#   for t in range(times_min, times_max + 1):
#     for i in tasks:
#       sol.add(Sum([(If(s[i] <= t,1,0) * If(t < s[i] + d[i],1,0))*r[i] for i in tasks])  <= b)

#   # Somewhat experimental:
#   # This constraint is needed to contrain the upper limit of b.
#   if not isinstance(b, int):
#     sol.add(b <= sum(r))


def main():

  # sol = Solver() # 25.140s
  sol = SolverFor("QF_FD") # 1.56s
  # sol = SolverFor("QF_LIA") # 10.67s

  #
  # data
  #
  n = 4
  duration = [30, 10, 15, 15]
  demand = [3, 1, 3, 2]
  max_end_time = 160

  #
  # declare variables
  #
  start_times = [ makeIntVar(sol,"start_times[%i]" % i, 0, max_end_time) for i in range(n)]
  end_times = [ makeIntVar(sol,"end_times[%i]" % i, 0, max_end_time) for i in range(n)]  
  end_time = makeIntVar(sol,"end_time", 0, max_end_time+max(duration))

  # number of needed resources, perhaps to be minimized
  num_resources = makeIntVar(sol, "num_resources", 0, 9)

  #
  # constraints
  #
  for i in range(n):
    sol.add(end_times[i] == start_times[i] + duration[i])

  maximum(sol, end_time, end_times)

  cumulative(sol, start_times, duration, demand, num_resources,0,max_end_time)

  #
  # Some extra constraints to play with
  #

  # all tasks must end within an hour
  sol.add(end_time <= 60)

  # All tasks should start at time 0
  # for i in range(n):
  #  sol.add(start_times[i] == 0)

  # limitation of the number of people
  # sol.add(num_resources <= 4)
  sol.add(num_resources <= 5)  

  #
  # objective
  # sol.minimize(end_time)
  # sol.minimize(num_resources)

  # solution and search
  # result
  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print("num_resources:", mod.eval(num_resources))
    print("demand       :", demand)
    print("start_times  :", [mod.eval(start_times[i]) for i in range(n)])
    print("duration     :", [duration[i] for i in range(n)])
    print("end_times    :", [mod.eval(end_times[i]) for i in range(n)])
    print("end_time     :", mod.eval(end_time))
    print()
    # getLessSolution(sol,mod,end_time)
    getLessSolution(sol,mod,num_resources+10*end_time) # "multi-objective"
    # getLessSolution(sol,mod,Sum(start_times))
    #getDifferentSolution(sol,mod,start_times,end_times)


  print()
  print("num_solutions:", num_solutions)

if __name__ == "__main__":
  main()


