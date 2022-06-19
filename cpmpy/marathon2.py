"""
Marathon puzzle in cpmpy.

From Xpress example
http://www.dashoptimization.com/home/cgi-bin/example.pl?id=mosel_puzzle_5_3
'''
Dominique, Ignace, Naren, Olivier, Philippe, and Pascal
have arrived as the first six at the Paris marathon.
Reconstruct their arrival order from the following
information:
a) Olivier has not arrived last
b) Dominique, Pascal and Ignace have arrived before Naren
  and Olivier
c) Dominique who was third last year has improved this year.
d) Philippe is among the first four.
e) Ignace has arrived neither in second nor third position.
f) Pascal has beaten Naren by three positions.
g) Neither Ignace nor Dominique are on the fourth position.

(c) 2002 Dash Associates
author: S. Heipcke, Mar. 2002
'''


This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *


def marathon2():

  model = Model()

  # data
  n = 6

  runners_str = [
      'Dominique', 'Ignace', 'Naren', 'Olivier', 'Philippe', 'Pascal'
  ]

  # declare variables
  runners = intvar(1,n,shape=n,name="runners")
  Dominique, Ignace, Naren, Olivier, Philippe, Pascal = runners

  # constraints
  model += [AllDifferent(runners)]

  # a: Olivier not last
  model += [Olivier != n]

  # b: Dominique, Pascal and Ignace before Naren and Olivier
  model += [Dominique < Naren]
  model += [Dominique < Olivier]
  model += [Pascal < Naren]
  model += [Pascal < Olivier]
  model += [Ignace < Naren]
  model += [Ignace < Olivier]

  # c: Dominique better than third
  model += [Dominique < 3]

  # d: Philippe is among the first four
  model += [Philippe <= 4]

  # e: Ignace neither second nor third
  model += [Ignace != 2]
  model += [Ignace != 3]

  # f: Pascal three places earlier than Naren
  model += [Pascal + 3 == Naren]

  # g: Neither Ignace nor Dominique on fourth position
  model += [Ignace != 4]
  model += [Dominique != 4]

  def print_sol():
    runners_val = runners.value()
    print('runners:', runners_val)
    print('Places:')
    for i in range(1, n + 1):
      for j in range(n):
        if runners_val[j] == i:
          print('%i: %s' % (i, runners_str[j]))
    print()
    

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print('num_solutions:', num_solutions)


marathon2()
