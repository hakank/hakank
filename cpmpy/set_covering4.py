"""
Set partition and set covering in cpmpy.

Example from the Swedish book
Lundgren, Roennqvist, Vaebrand
'Optimeringslaera' (translation: 'Optimization theory'),
page 408.

* Set partition:
  We want to minimize the cost of the alternatives which covers all the
  objects, i.e. all objects must be choosen. The requirement is than an
  object may be selected _exactly_ once.

  Note: This is 1-based representation

  Alternative        Cost        Object
  1                  19           1,6
  2                  16           2,6,8
  3                  18           1,4,7
  4                  13           2,3,5
  5                  15           2,5
  6                  19           2,3
  7                  15           2,3,4
  8                  17           4,5,8
  9                  16           3,6,8
  10                 15           1,6,7

  The problem has a unique solution of z = 49 where alternatives
    3, 5, and 9
  is selected.

* Set covering:
  If we, however, allow that an object is selected _more than one time_,
  then the solution is z = 45 (i.e. less cost than the first problem),
  and the alternatives
    4, 8, and 10
  is selected, where object 5 is selected twice (alt. 4 and 8).
  It's an unique solution as well.


This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *




def set_covering4(set_partition=1):


  #
  # data
  #
  num_alternatives = 10
  num_objects = 8

  # costs for the alternatives
  costs = [19, 16, 18, 13, 15, 19, 15, 17, 16, 15]

  # the alternatives, and their objects
  a = [
      # 1 2 3 4 5 6 7 8    the objects
      [1, 0, 0, 0, 0, 1, 0, 0],  # alternative 1
      [0, 1, 0, 0, 0, 1, 0, 1],  # alternative 2
      [1, 0, 0, 1, 0, 0, 1, 0],  # alternative 3
      [0, 1, 1, 0, 1, 0, 0, 0],  # alternative 4
      [0, 1, 0, 0, 1, 0, 0, 0],  # alternative 5
      [0, 1, 1, 0, 0, 0, 0, 0],  # alternative 6
      [0, 1, 1, 1, 0, 0, 0, 0],  # alternative 7
      [0, 0, 0, 1, 1, 0, 0, 1],  # alternative 8
      [0, 0, 1, 0, 0, 1, 0, 1],  # alternative 9
      [1, 0, 0, 0, 0, 1, 1, 0]  # alternative 10
  ]
  
  # For slighly neater encodings using numpy below
  at = np.array(a).transpose()

  #
  # declare variables
  #
  x = boolvar(shape=num_alternatives,name="x")
  z = intvar(0,100,name="z")

  model = Model(minimize=z)

  #
  # constraints
  #

  # sum the cost of the choosen alternative,
  # to be minimized
  model += [z == sum(x*costs)]

  #
  for j in range(num_objects):
    if set_partition == 1:
      # model += [ sum([x[i] * a[i][j] for i in range(num_alternatives)]) == 1]
      model += [ sum(x * at[j]) == 1]
    else:
      # model += [sum([x[i] * a[i][j] for i in range(num_alternatives)]) >= 1]
      model += [ sum(x * at[j]) >= 1]

  ss = CPM_ortools(model)

  alternatives = np.array(range(num_alternatives))+1
  if ss.solve():
    print("z:", z.value())
    # print("selected alternatives:", [i + 1 for i in range(num_alternatives) if x[i].value() == 1])
    print("selected alternatives:", alternatives[x.value() == 1])

print("Set partition:")
set_covering4(1)

print("\nSet covering:")
set_covering4(0)
