"""
Set partition problem in cpmpy.

Problem formulation from
http://www.koalog.com/resources/samples/PartitionProblem.java.html
'''
  This is a partition problem.
  Given the set S = {1, 2, ..., n},
  it consists in finding two sets A and B such that:

    A U B = S,
    |A| = |B|,
    sum(A) = sum(B),
    sum_squares(A) = sum_squares(B)

'''

This model uses a binary matrix to represent the sets.

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *




#
# Partition the sets (binary matrix representation).
#
def partition_sets(x, num_sets, n):

  constraints = []
  for i in range(num_sets):
    for j in range(num_sets):
      if i != j:
        # b = solver.Sum([x[i, k] * x[j, k] for k in range(n)])
        # solver.Add(b == 0)
        constraints += [sum([x[i, k] * x[j, k] for k in range(n)]) == 0]

  # ensure that all integers is in (exactly) one partition
  # b = [x[i, j] for i in range(num_sets) for j in range(n)]
  constraints += [sum([x[i, j] for i in range(num_sets) for j in range(n)]) == n]

  return constraints


def set_partition_model(n=16, num_sets=2):

  model = Model()

  
  # data
  print("n:", n)
  print("num_sets:", num_sets)
  print()

  # Check sizes
  assert n % num_sets == 0, "Equal sets is not possible."


  # variables


  # the set
  # a = {}
  # for i in range(num_sets):
  #   for j in range(n):
  #     a[i, j] = solver.IntVar(0, 1, "a[%i,%i]" % (i, j))

  # a_flat = [a[i, j] for i in range(num_sets) for j in range(n)]

  a = boolvar(shape=(num_sets,n),name="a")

  #
  # constraints
  #

  # partition set
  model += [partition_sets(a, num_sets, n)]

  for i in range(num_sets):
    for j in range(i, num_sets):

      # same cardinality
      model += [sum([a[i, k] for k in range(n)]) ==
                sum([a[j, k] for k in range(n)])]

      # same sum
      model += [sum([k * a[i, k] for k in range(n)]) ==
                sum([k * a[j, k] for k in range(n)])]

      # same sum squared
      model += [sum([(k * a[i, k]) * (k * a[i, k]) for k in range(n)]) ==
                sum([(k * a[j, k]) * (k * a[j, k]) for k in range(n)])]

  # symmetry breaking for num_sets == 2
  if num_sets == 2:
    model += [a[0, 0] == 1]

  def print_sol():
    a_val = {}
    for i in range(num_sets):
      for j in range(n):
        a_val[i, j] = a[i, j].value()

    sq = sum([(j + 1) * a_val[0, j] for j in range(n)])
    print("sums:", sq)
    sq2 = sum([((j + 1) * a_val[0, j])**2 for j in range(n)])
    print("sums squared:", sq2)

    for i in range(num_sets):
      if sum([a_val[i, j] for j in range(n)]):
        print(i + 1, ":", end=" ")
        for j in range(n):
          if a_val[i, j] == 1:
            print(j + 1, end=" ")
        print()
    print()
    

  ss = CPM_ortools(model)
  # Flags to experiment with
  # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
  # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
  # ss.ort_solver.parameters.cp_model_presolve = False
  ss.ort_solver.parameters.linearization_level = 0
  ss.ort_solver.parameters.cp_model_probing_level = 0
  
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:",num_solutions)


n = 16
num_sets = 2
if len(sys.argv) > 1:
  n = int(sys.argv[1])
if len(sys.argv) > 2:
  num_sets = int(sys.argv[2])

set_partition_model(n, num_sets)
