"""
Car sequencing in cpmpy.

This model is based on the car sequencing model in
Pascal Van Hentenryck
'The OPL Optimization Programming Language', page 184ff.

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *



def car(num_sol=3):

  model = Model()
  
  #
  # data
  #
  nbCars = 6
  nbOptions = 5
  nbSlots = 10

  Cars = list(range(nbCars))
  Options = list(range(nbOptions))
  Slots = list(range(nbSlots))

  #    car 0   1  2  3  4  5
  demand = [1, 1, 2, 2, 2, 2]

  option = [
      # car 0  1  2  3  4  5
      [1, 0, 0, 0, 1, 1],  # option 1
      [0, 0, 1, 1, 0, 1],  # option 2
      [1, 0, 0, 0, 1, 0],  # option 3
      [1, 1, 0, 1, 0, 0],  # option 4
      [0, 0, 1, 0, 0, 0]  # option 5
  ]

  capacity = [(1, 2), (2, 3), (1, 3), (2, 5), (1, 5)]

  optionDemand = [
      sum([demand[j] * option[i][j] for j in Cars]) for i in Options
  ]

  #
  # declare variables
  #
  slot = intvar(0,nbCars-1,shape=nbSlots,name="slot")
  setup = boolvar(shape=(nbOptions,nbSlots),name="setup")

  #
  # constraints
  #
  for c in Cars:
    model += (sum([slot[s] == c for s in Slots]) == demand[c])

  for o in Options:
    for s in range(0, nbSlots - capacity[o][1]+1):
      model += (sum([setup[o, j] for j in range(s, s + capacity[o][1]-1)]) <= capacity[o][0])

  for o in Options:
    for s in Slots:
      model += setup[(o, s)] == Element(option[o],slot[s])

  for o in Options:
    for i in range(optionDemand[o]):
      s_range = list(range(0, nbSlots - (i+1)*capacity[o][1]))
      setup_s = [setup[o, s] for s in s_range]
      cc = optionDemand[o]- (i+1)*capacity[o][0]
      if len(setup_s) > 0 and cc >= 0:
        model += (sum(setup_s) >= cc)

  def print_sol():
    print("slot:%s" % ",".join([str(slot[i].value()) for i in Slots]))
    print("setup:")
    for o in Options:
      print("%i/%i:" % (capacity[o][0], capacity[o][1]), end=" ")
      for s in Slots:
        print(int(setup[o, s].value()), end=" ")
      print()
    print()

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(solution_limit=num_sol,display=print_sol)
  print()
  print("num_solutions:", num_solutions)

num_sol = 3
if len(sys.argv) > 1:
  num_sol = int(sys.argv[1])
car(num_sol)
