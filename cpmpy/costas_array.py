"""
Costas array in cpmpy.

From http://mathworld.wolfram.com/CostasArray.html:
'''
An order-n Costas array is a permutation on {1,...,n} such
that the distances in each row of the triangular difference
table are distinct. For example, the permutation {1,3,4,2,5}
has triangular difference table {2,1,-2,3}, {3,-1,1}, {1,2},
and {4}. Since each row contains no duplications, the permutation
is therefore a Costas array.
'''

Also see
http://en.wikipedia.org/wiki/Costas_array

About this model:
This model is based on Barry O'Sullivan's model:
http://www.g12.cs.mu.oz.au/mzn/costas_array/CostasArray.mzn

and my small changes in
http://hakank.org/minizinc/costas_array.mzn

Since there is no symmetry breaking of the order of the Costas
array it gives all the solutions for a specific length of
the array, e.g. those listed in
http://mathworld.wolfram.com/CostasArray.html

1     1       (1)
2     2       (1, 2), (2,1)
3     4       (1, 3, 2), (2, 1, 3), (2, 3, 1), (3, 1, 2)
4     12      (1, 2, 4, 3), (1, 3, 4, 2), (1, 4, 2, 3), (2, 1, 3, 4),
              (2, 3, 1, 4), (2, 4, 3, 1), (3, 1, 2, 4), (3, 2, 4, 1),
              (3, 4, 2, 1), (4, 1, 3, 2), (4, 2, 1, 3), (4, 3, 1, 2)
....

See http://www.research.att.com/~njas/sequences/A008404
for the number of solutions for n=1..
1, 2, 4, 12, 40, 116, 200, 444, 760, 2160, 4368, 7852, 12828,
17252, 19612, 21104, 18276, 15096, 10240, 6464, 3536, 2052,
872, 200, 88, 56, 204,...

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *


def costas_array(n=6,print_solutions=True):

  model = Model()

  #
  # data
  #
  print("n:", n)

  #
  # declare variables
  #
  costas = intvar(1,n,shape=n,name="costas")
  differences = intvar(-n+1,n-1,shape=(n,n),name="differences")

  #
  # constraints
  #

  # Fix the values in the lower triangle in the
  # difference matrix to -n+1. This removes variants
  # of the difference matrix for the the same Costas array.
  for i in range(n):
    for j in range(i + 1):
      model += [differences[i,j] == -n + 1]

  # hakank: All the following constraints are from
  # Barry O'Sullivans's original model.
  #
  model += [AllDifferent(costas)]

  # "How do the positions in the Costas array relate
  #  to the elements of the distance triangle."
  for i in range(n):
    for j in range(n):
      if i < j:
        model += [differences[(i,j)] == costas[j] - costas[j - i - 1]]

  # "All entries in a particular row of the difference
  #  triangle must be distint."
  for i in range(n - 2):
    model += [AllDifferent([differences[i,j] for j in range(n) if j > i])]

  #
  # "All the following are redundant - only here to speed up search."
  #

  # "We can never place a 'token' in the same row as any other."
  for i in range(n):
    for j in range(n):
      if i < j:
        model += [differences[i,j] != 0]

  for k in range(2, n):
    for l in range(2, n):
      if k < l:
        model += [differences[k-2,l-1] + differences[k,l] ==
                   differences[k-1,l-1] + differences[k-1,l]]

  def print_sol():
    if print_solutions:
      print("costas:", costas.value())
      print("differences:")
      for i in range(n):
        for j in range(n):
          v = differences[(i,j)].value()
          if v == -n + 1:
            print("  ", end=" ")
          else:
            print("%2d" % v, end=" ")
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
  print("Nr solutions:", num_solutions)
  if print_solutions:
    print(ss.status())
    print("Num conflicts:", ss.ort_solver.NumConflicts())
    print("NumBranches:", ss.ort_solver.NumBranches())
    print("WallTime:", ss.ort_solver.WallTime())

  return num_solutions

n = 6
print_solutions=True
if len(sys.argv) > 1:
  n = int(sys.argv[1])
costas_array(n,print_solutions)

num_sols = []
for n in range(2,10):
  num_sols.append(costas_array(n,False))
  print()
print(num_sols)
  
