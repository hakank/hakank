"""
Broken weights problem in cpmpy.

From http://www.mathlesstraveled.com/?p=701
'''
Here's a fantastic problem I recently heard. Apparently it was first
posed by Claude Gaspard Bachet de Meziriac in a book of arithmetic problems
published in 1612, and can also be found in Heinrich Dorrie's 100
Great Problems of Elementary Mathematics.

  A merchant had a forty pound measuring weight that broke
  into four pieces as the result of a fall. When the pieces were
  subsequently weighed, it was found that the weight of each piece
  was a whole number of pounds and that the four pieces could be
  used to weigh every integral weight between 1 and 40 pounds. What
  were the weights of the pieces?

Note that since this was a 17th-century merchant, he of course used a
balance scale to weigh things. So, for example, he could use a 1-pound
weight and a 4-pound weight to weigh a 3-pound object, by placing the
3-pound object and 1-pound weight on one side of the scale, and
the 4-pound weight on the other side.
'''

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *



def broken_weights(m=40, n=4):

  # data
  print('total weight (m):', m)
  print('number of pieces (n):', n)
  print()

  # variables
  weights = intvar(1,m, shape=n,name="weights")
  x = intvar(-1,1,shape=(m,n),name="x")

  model = Model(minimize=weights[-1])
  # model = Model()

  # constraints

  model += [AllDifferent(weights)]
  model += [sum(weights) == m]

  # Check that all weights from 1 to 40 can be made.
  #
  # Since all weights can be on either side
  # of the side of the scale we allow either
  # -1, 0, or 1 of the weights, assuming that
  # -1 is the weights on the left and 1 is on the right.
  #
  for i in range(m):
    model += [sum([weights[j] * x[i,j] for j in range(n)]) == i+1]
    # model += [i+1 == sum(weights * x[i])] # using numpy's magic

  # symmetry breaking
  # for j in range(1, n):
  #   model += [weights[j-1] < weights[j]]
  model += [increasing_strict(weights)]

  ss = CPM_ortools(model)
  # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
  # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
  # ss.ort_solver.parameters.cp_model_presolve = False
  # ss.ort_solver.parameters.linearization_level = 0
  # ss.ort_solver.parameters.cp_model_probing_level = 0

  num_solutions = 0
  if ss.solve():
    num_solutions += 1
    print('weights:   ', end=' ')
    for w in [weights[j].value() for j in range(n)]:
      print('%3i ' % w, end=' ')
    print()
    print('-' * 30)
    for i in range(m):
      print('weight  %2i:' % (i + 1), end=' ')
      for j in range(n):
        print('%3i ' % x[i, j].value(), end=' ')
      print()
    print()
  print()

  print('num_solutions:', num_solutions)


m = 40
n = 4
if len(sys.argv) > 1:
  m = int(sys.argv[1])
if len(sys.argv) > 2:
  n = int(sys.argv[2])
broken_weights(m, n)
