"""
Post office problem in cpmpy.

Problem statement:
http://www-128.ibm.com/developerworks/linux/library/l-glpk2/

From Winston 'Operations Research: Applications and Algorithms':
'''
A post office requires a different number of full-time employees working
on different days of the week [summarized below]. Union rules state that
each full-time employee must work for 5 consecutive days and then receive
two days off. For example, an employee who works on Monday to Friday
must be off on Saturday and Sunday. The post office wants to meet its
daily requirements using only full-time employees. Minimize the number
of employees that must be hired.

To summarize the important information about the problem:

  * Every full-time worker works for 5 consecutive days and takes 2 days off
  * Day 1 (Monday): 17 workers needed
  * Day 2 : 13 workers needed
  * Day 3 : 15 workers needed
  * Day 4 : 19 workers needed
  * Day 5 : 14 workers needed
  * Day 6 : 16 workers needed
  * Day 7 (Sunday) : 11 workers needed

The post office needs to minimize the number of employees it needs
to hire to meet its demand.
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *



def post_office_problem2():

  
  #
  # data
  #

  # days 0..6, monday 0
  n = 7
  days = list(range(n))
  need = [17, 13, 15, 19, 14, 16, 11]

  # Total cost for the 5 day schedule.
  # Base cost per day is 100.
  # Working saturday is 100 extra
  # Working sunday is 200 extra.
  cost = [500, 600, 800, 800, 800, 800, 700]

  #
  # variables
  #

  # Number of workers starting at day i
  x = intvar(0,100,shape=n,name="x")

  total_cost = intvar(0, 20000, name="total_cost")
  num_workers = intvar(0, 100, name="num_workers")

  model = Model(minimize=total_cost)

  #
  # constraints
  #
  model += [total_cost == sum(x*cost)]
  model += [num_workers == sum(x)]

  for i in days:
    model += [sum([x[j] for j in days
                   if j != (i + 5) % n and j != (i + 6) % n])
              >= need[i]]

  ss = CPM_ortools(model)
  num_solutions = 0
  if ss.solve():
    num_solutions += 1
    print("num_workers:", num_workers.value())
    print("total_cost:", total_cost.value())
    print("x:", x.value())



post_office_problem2()
