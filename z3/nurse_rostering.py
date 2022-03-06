#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Nurse rostering problem in Z3
#
# This is a simple nurse rostering model using a DFA and
# my decomposition of regular constraint.
#
#  The DFA is from MiniZinc Tutorial, Nurse Rostering example:
# - one day off every 4 days
# - no 3 nights in a row.
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
from collections import defaultdict
from z3_utils_hakank import *



def main():

  sol = SolverFor("QF_FD")
  # sol = SolverFor("QF_AUFLIA")
  # sol = SimpleSolver()

  # data

  # Note: If you change num_nurses or num_days,
  #       please also change the constraints
  #       on nurse_stat and/or day_stat.
  num_nurses = 7
  num_days = 14

  day_shift = 1
  night_shift = 2
  off_shift = 3
  shifts = [day_shift, night_shift, off_shift]

  # the DFA (for regular)
  n_states = 6
  input_max = 3
  initial_state = 1  # 0 is for the failing state
  accepting_states = [1, 2, 3, 4, 5, 6]

  transition_fn = [
      # d,n,o
      [2, 3, 1], # state 1
      [4, 4, 1], # state 2
      [4, 5, 1], # state 3
      [6, 6, 1], # state 4
      [6, 0, 1], # state 5
      [0, 0, 1]  # state 6
  ]

  days = ['d', 'n', 'o']  # for presentation

  #
  # declare variables
  #
  x = {}
  for i in range(num_nurses):
    for j in range(num_days):
      x[i, j] = makeIntVar(sol, 'x[%i,%i]' % (i, j), day_shift, off_shift)

  x_flat = [x[i, j] for i in range(num_nurses) for j in range(num_days)]

  # summary of the nurses
  nurse_stat = [makeIntVar(sol, 'nurse_stat[%i]' % i, 0, num_days)
                for i in range(num_nurses)]

  # summary of the shifts per day
  day_stat = {}
  for i in range(num_days):
    for j in shifts:
      day_stat[i, j] = makeIntVar(sol, 'day_stat[%i,%i]' % (i, j), 0, num_nurses)

  day_stat_flat = [day_stat[i, j] for i in range(num_days) for j in shifts]

  #
  # constraints
  #
  for i in range(num_nurses):
    reg_input = [x[i, j] for j in range(num_days)]
    regular2(sol, reg_input, n_states, input_max, transition_fn,
            initial_state, accepting_states, len(reg_input))

  #
  # Statistics and constraints for each nurse
  #
  for i in range(num_nurses):
    # number of worked days (day or night shift)
    sol.add(nurse_stat[i] ==
               Sum([If(x[i, j]==day_shift,1,0) +
                    If(x[i, j]==night_shift,1,0)
                    for j in range(num_days)]
                                    ))

    # Each nurse must work between 7 and 10
    # days during this period
    sol.add(nurse_stat[i] >= 7)
    sol.add(nurse_stat[i] <= 10)

  #
  # Statistics and constraints for each day
  #
  for j in range(num_days):
    for t in shifts:
      sol.add(day_stat[j, t] ==
                 Sum([If(x[i, j]==t,1,0)
                      for i in range(num_nurses)]))

    #
    # Some constraints for this day:
    #
    # Note: We have a strict requirements of
    #       the number of shifts.
    #       Using atleast constraints is much harder
    #       in this model.
    #
    if j % 7 == 5 or j % 7 == 6:
      # special constraints for the weekends
      sol.add(day_stat[j, day_shift] == 2)
      sol.add(day_stat[j, night_shift] == 1)
      sol.add(day_stat[j, off_shift] == 4)
    else:
      # workdays:

      # - exactly 3 on day shift
      sol.add(day_stat[j, day_shift] == 3)
      # - exactly 2 on night
      sol.add(day_stat[j, night_shift] == 2)
      # - exactly 1 off duty
      sol.add(day_stat[j, off_shift] == 2)

  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    for i in range(num_nurses):
      print('Nurse%i: ' % i, end=' ')
      this_day_stat = defaultdict(int)
      for j in range(num_days):
        d = days[mod.eval(x[i, j]).as_long() - 1]
        this_day_stat[d] += 1
        print(d, end=' ')
      print(' day_stat:', [(d, this_day_stat[d]) for d in this_day_stat], end=' ')
      print('total:', mod.eval(nurse_stat[i]), 'workdays')
    print()

    print('Statistics per day:')
    for j in range(num_days):
      print('Day%2i: ' % j, end=' ')
      for t in shifts:
        print(mod.eval(day_stat[j, t]), end=' ')
      print()
    print()

    # We just show 2 solutions
    if num_solutions >= 2:
      break

  print()
  print('num_solutions:', num_solutions)


if __name__ == '__main__':
  main()
