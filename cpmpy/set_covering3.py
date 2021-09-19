"""
Set covering in cpmpy

Problem from
Katta G. Murty: 'Optimization Models for Decision Making', page 302f
http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf

10 senators making a committee, where there must at least be one
representative from each group:
group:        senators:
southern      1 2 3 4 5
northern      6 7 8 9 10
liberals      2 3 8 9 10
conservative  1 5 6 7
democrats     3 4 5 6 7 9
republicans   1 2 8 10

The objective is to minimize the number of senators.

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *



def set_covering3():


  #
  # data
  #
  num_groups = 6
  num_senators = 10

  # which group does a senator belong to?
  belongs = [
      [1, 1, 1, 1, 1, 0, 0, 0, 0, 0],  # 1 southern
      [0, 0, 0, 0, 0, 1, 1, 1, 1, 1],  # 2 northern
      [0, 1, 1, 0, 0, 0, 0, 1, 1, 1],  # 3 liberals
      [1, 0, 0, 0, 1, 1, 1, 0, 0, 0],  # 4 conservative
      [0, 0, 1, 1, 1, 1, 1, 0, 1, 0],  # 5 democrats
      [1, 1, 0, 0, 0, 0, 0, 1, 0, 1]   # 6 republicans
  ]
  belongs_t = np.array(belongs).transpose()
  
  #
  # declare variables
  #
  x = boolvar(shape=num_senators,name="x")
  z = intvar(0,10,name="z")

  #
  # constraints
  #
  model = Model(minimize=z)

  # number of assigned senators (to minimize)
  model += [z == sum(x)]

  # ensure that each group is covered by at least
  # one senator
  for i in range(num_groups):
    # model += [sum([x[j] * belongs[i][j] for j in range(num_senators)]) == 1]
    model += [sum(x * belongs[i]) == 1]


  ss = CPM_ortools(model)
  if ss.solve():
    print("z:", z.value())
    print("x:", x.value())
    groups = np.array(list(range(num_groups)))+1
    for j in range(num_senators):
      if x[j].value() == 1:
        print("Senator", j + 1, "belongs to these groups:", end=" ")
        print(groups[belongs_t[j] == 1])

  print()


set_covering3()
