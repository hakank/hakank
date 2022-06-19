"""
Remarkable sequence  in cpmpy.

Problem statement in the Alma-0 example program remarkable.a0
'''
This problem is taken from
@book{CC88,
      author = 'H. Coelho and J. C. Cotta',
      title = 'Prolog by Example',
      publisher = 'Springer-Verlag',
      address = 'Berlin',
      year = 1988}
(page 193)

Call a sequence of 27 elements remarkable if it consists of three 1's,
three 2's, ...  three 9's arranged in such a way that for all i in
[1..9] there are exactly i numbers between successive occurrences of
i.  For example, the sequence

(1,9,1,2,1,8,2,4,6,2,7,9,4,5,8,6,3,4,7,5,3,9,6,8,3,5,7)

is remarkable.  Write a program that generates all
remarkable sequences.
'''

There are three solution (with the symmetry breaking that 
the first element must be less than the last element):

  a: [1 8 1 9 1 5 2 6 7 2 8 5 2 9 6 4 7 5 3 8 4 6 3 9 7 4 3]
  a: [1 9 1 6 1 8 2 5 7 2 6 9 2 5 8 4 7 6 3 5 4 9 3 8 7 4 3]
  a: [1 9 1 2 1 8 2 4 6 2 7 9 4 5 8 6 3 4 7 5 3 9 6 8 3 5 7]

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *
from collections import defaultdict



def remarkable_sequence():

  n = 9 # the digits
  m = 3  # number of occurrences of each number
  nm = n*m
  
  # variables
  a = intvar(1,n,shape=n*m,name="a")

  model = Model()

  js = []
  for i in range(1,n+1):
    j = intvar(0,n*m-1,name=f"j[{i}]")
    js.append(j)
    for k in range(m):
      model += [a[j+(i*k)+k] == i]

  model += [AllDifferent(js)]

  #
  # Here are three ways to write the count constraint:
  #
  
  # for i in range(1,n+1):
  #  model += [count(a,i,m)]

  # Note: The gcc must also include the value for 0..ub
  # gcc = [0] + [m for _ in range(1,n+1)]
  # model += [global_cardinality_count(a.flat,gcc)]

  # Using distribute instead (seems to be the fastest)
  distribute([m for _ in range(n)], list(range(1,n+1)), a.flat)
    
  # Symmetry breaking
  model += [a[0] <= a[-1]]

  ss = CPM_ortools(model)
  # ss.ort_solver.parameters.log_search_progress = True
  # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
  # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
  # ss.ort_solver.parameters.cp_model_presolve = False
  ss.ort_solver.parameters.linearization_level = 0
  ss.ort_solver.parameters.cp_model_probing_level = 0

  num_solutions = ss.solveAll(display=a)
  print("num_solutions:", num_solutions)


remarkable_sequence()
