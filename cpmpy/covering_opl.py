"""
Set covering problem in cpmpy.

This example is from the OPL example covering.mod
'''
Consider selecting workers to build a house. The construction of a
house can be divided into a number of tasks, each requiring a number of
skills (e.g., plumbing or masonry). A worker may or may not perform a
task, depending on skills. In addition, each worker can be hired for a
cost that also depends on his qualifications. The problem consists of
selecting a set of workers to perform all the tasks, while minimizing the
cost. This is known as a set-covering problem. The key idea in modeling
a set-covering problem as an integer program is to associate a 0/1
variable with each worker to represent whether the worker is hired.
To make sure that all the tasks are performed, it is sufficient to
choose at least one worker by task. This constraint can be expressed by a
simple linear inequality.
'''

Solution from the OPL model (1-based)
'''
Optimal solution found with objective: 14
crew= {23 25 26}
'''

Solution from this model (0-based):
'''
Total cost 14
We should hire these workers:  22 24 25
'''


This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *




def covering_opl():



  # data
  nb_workers = 32
  Workers = list(range(nb_workers))
  num_tasks = 15
  Tasks = list(range(num_tasks))

  # Which worker is qualified for each task.
  # Note: This is 1-based and will be made 0-base below.
  Qualified = [[1, 9, 19, 22, 25, 28, 31],
               [2, 12, 15, 19, 21, 23, 27, 29, 30, 31, 32],
               [3, 10, 19, 24, 26, 30, 32], [4, 21, 25, 28, 32],
               [5, 11, 16, 22, 23, 27, 31], [6, 20, 24, 26, 30, 32],
               [7, 12, 17, 25, 30, 31], [8, 17, 20, 22, 23],
               [9, 13, 14, 26, 29, 30, 31], [10, 21, 25, 31, 32],
               [14, 15, 18, 23, 24, 27, 30, 32], [18, 19, 22, 24, 26, 29, 31],
               [11, 20, 25, 28, 30, 32], [16, 19, 23, 31],
               [9, 18, 26, 28, 31, 32]]

  Cost = [
      1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5,
      5, 6, 6, 6, 7, 8, 9
  ]

  #
  # variables
  #
  Hire = boolvar(shape=nb_workers,name="Hire")
  total_cost = intvar(0,nb_workers * sum(Cost),name="total_cost")

  model = Model(minimize=total_cost)

  #
  # constraints
  #
  model += [total_cost == sum(Hire*Cost)]

  for j in Tasks:
    # Sum the cost for hiring the qualified workers
    # (also, make 0-base)
    model += [sum([Hire[c - 1] for c in Qualified[j]]) >= 1]

  ss = CPM_ortools(model)
  num_solutions = 0
  if ss.solve():
    num_solutions += 1
    print("Total cost", total_cost.value())
    print("We should hire these workers: ", end=" ")
    for w in Workers:
      if Hire[w].value() == 1:
        print(w, end=" ")
    print()
    print()


covering_opl()
