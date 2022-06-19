"""
Nurse rostering in cpmpy.

This is a simple nurse rostering model using a DFA and
my decomposition of regular constraint.

The DFA is from MiniZinc Tutorial, Nurse Rostering example:
- one day off every 4 days
- no 3 nights in a row.


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *
from collections import defaultdict



def nurse_rostering(num_sols=2):

  model = Model()

  #
  # data
  #

  # Note: If you change num_nurses or num_days,
  #       please also change the constraints
  #       on nurse_stat and/or day_stat.
  num_nurses = 7
  num_days = 14

  day_shift = 1
  night_shift = 2
  off_shift = 3
  num_shifts = 3
  shifts = [day_shift, night_shift, off_shift]

  # the DFA (for regular)
  n_states = 6
  input_max = 3
  initial_state = 1  # 0 is for the failing state
  accepting_states = [1, 2, 3, 4, 5, 6]

  transition_fn = [
      # d,n,o
      [2, 3, 1],  # state 1
      [4, 4, 1],  # state 2
      [4, 5, 1],  # state 3
      [6, 6, 1],  # state 4
      [6, 0, 1],  # state 5
      [0, 0, 1]  # state 6
  ]

  days = ['d', 'n', 'o']  # for presentation

  #
  # declare variables
  #
  x = {}
  for i in range(num_nurses):
    for j in range(num_days):
      x[i, j] = intvar(day_shift, off_shift, name='x[%i,%i]' % (i, j))
  x_flat = [x[i,j] for i in range(num_nurses) for j in range(num_days)]

  # summary of the nurses
  nurse_stat = [
      intvar(0, num_days, name='nurse_stat[%i]' % i)
      for i in range(num_nurses)
  ]

  
  # summary of the shifts per day
  day_stat = {}
  for i in range(num_days):
    for j in shifts:
      day_stat[i,j] = intvar(0, num_nurses, name='day_stat[%i,%i]' % (i, j))

  #
  # constraints
  #
  for i in range(num_nurses):
    reg_input = [x[i,j] for j in range(num_days)]
    model += [regular(reg_input, n_states, input_max, transition_fn, initial_state,
                      accepting_states)]

  #
  # Statistics and constraints for each nurse
  #
  for i in range(num_nurses):
    # number of worked days (day or night shift)
    b = [(x[i,j]==day_shift) + (x[i,j]==night_shift) for j in range(num_days)]
    model += [nurse_stat[i] == sum(b)]

    # Each nurse must work between 7 and 10
    # days during this period
    model += [nurse_stat[i] >= 7,
              nurse_stat[i] <= 10]

  #
  # Statistics and constraints for each day
  #
  for j in range(num_days):
    for t in shifts:
      b = [x[i,j] == t for i in range(num_nurses)]
      model += [day_stat[j, t] == sum(b)]

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
      model += [day_stat[j, day_shift]   == 2,
                day_stat[j, night_shift] == 1,
                day_stat[j, off_shift]   == 4]
      
    else:
      # workdays:
      # - exactly 3 on day shift
      # - exactly 2 on night
      # - exactly 1 off duty      
      model += [day_stat[j, day_shift]   == 3,
                day_stat[j, night_shift] == 2,
                day_stat[j, off_shift]   == 2]

  ss = CPM_ortools(model)
  # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
  # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
  # ss.ort_solver.parameters.cp_model_presolve = False
  ss.ort_solver.parameters.linearization_level = 0
  ss.ort_solver.parameters.cp_model_probing_level = 0

  def print_sol():
    for i in range(num_nurses):
      print('Nurse%i: ' % i, end=' ')
      this_day_stat = defaultdict(int)
      for j in range(num_days):
        d = days[x[i, j].value() - 1]
        this_day_stat[d] += 1
        print(d, end=' ')
      print(
          ' day_stat:', [(d, this_day_stat[d]) for d in this_day_stat], end=' ')
      print('total:', nurse_stat[i].value(), 'workdays')
    print()

    print('Statistics per day:')
    for j in range(num_days):
      print('Day%2i: ' % j, end=' ')
      for t in shifts:
        print(day_stat[j, t].value(), end=' ')
      print()
    print()


  num_solutions = ss.solveAll(solution_limit=num_sols,display=print_sol)
  print('num_solutions:', num_solutions)

num_sols = 2
nurse_rostering(num_sols)
