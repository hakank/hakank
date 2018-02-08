#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Bus scheduling problem in Z3
#
# Minimize the number of buses needed for the schedule.
#
# Problem from Taha "Introduction to Operations Research", page 58.
# This is a slightly more general model than Taha's.
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
from z3_utils_hakank import *


def main(num_buses_check=0):
  print("num_buses_check", num_buses_check)
  if num_buses_check == 0:
      sol = Optimize()
  else:
      sol = Solver()

  # data
  time_slots = 6
  demands = [8, 10, 7, 12, 4, 4]
  max_num = sum(demands)

  # declare variables
  x = [makeIntVar(sol,"x%i" % i,0, max_num) for i in range(time_slots)]
  num_buses = makeIntVar(sol,"num_buses", 0, max_num)

  # constraints
  sol.add(num_buses == Sum(x))

  # Meet the demands for this and the next time slot
  for i in range(time_slots - 1):
    sol.add(x[i] + x[i+1] >= demands[i])

  # The demand "around the clock"
  sol.add(x[time_slots-1] + x[0] == demands[time_slots-1])

  # solution and search

  if num_buses_check == 0:
    sol.minimize(num_buses)
  else:
    sol.add(num_buses == num_buses_check)

  if num_buses_check == 0:
    # check for optimal value
    if sol.check() == sat:
        mod = sol.model()
        print("x:", [mod.eval(x[i]) for i in range(len(x))], end=' ')
        num_buses_check_value = mod.eval(num_buses)
        print(" num_buses:", num_buses_check_value)
  else:
      # all solutions
      num_solutions = 0
      while sol.check() == sat:
          num_solutions += 1
          mod = sol.model()
          print("x:", [mod.eval(x[i]) for i in range(len(x))], end=' ')
          print(" num_buses:", mod.eval(num_buses))
          getDifferentSolution(sol,mod,x)
      print()
      print("num_solutions:", num_solutions)
  if num_buses_check == 0:
    return num_buses_check_value

if __name__ == "__main__":
  print("Check for minimun number of buses")
  num_buses_check = main()
  print("... got ", num_buses_check, "buses")
  print("All solutions:")
  main(num_buses_check)
