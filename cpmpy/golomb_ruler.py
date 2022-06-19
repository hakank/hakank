"""
Golomb's ruler problem in cpmpy.

A Golomb ruler is a set of integers (marks) a(1) < ...  < a(n) such
that all the differences a(i)-a(j) (i > j) are distinct.  Clearly we
may assume a(1)=0.  Then a(n) is the length of the Golomb ruler.
For a given number of marks, n, we are interested in finding the
shortest Golomb rulers.  Such rulers are called optimal. 

Also, see: 
- https://en.wikipedia.org/wiki/Golomb_ruler
- http://www.research.ibm.com/people/s/shearer/grule.html


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *



def golomb(size=8):

  print("size:",size)

  var_max = size*size
  all_vars = list(range(0,size))

  marks = intvar(0,var_max,shape=size,name="marks")

  model = Model(minimize=marks[size-1])

  model += (marks[0] == 0)
  model += (increasing_strict(marks))

  # model += [AllDifferent([marks[j] - marks[i]
  #                   for i in range(0, size-1)
  #                   for j in range(i+1, size)])]
  diffs = []
  for i in range(0, size-1):
    for j in range(i+1,size):
      d = intvar(0,var_max,name=f"d[{i,j}]")
      model += (d == marks[j]-marks[i])
      diffs.append(d)
  model += [AllDifferent(diffs)]
  
  
  # Symmetry breaking
  model += (marks[size-1] - marks[size-2] > marks[1] - marks[0])
  model += (diffs[0] < diffs[size-1])

  # Inspired by 
  # Barbara M. Smith, Kostas Stergiou, and Toby Walsh:
  # "Modelling the Golomb Ruler Problem"
  for i in range(size):
    for j in range(size):
      for k in range(size):
        if i < j and j < k:
          model += (2*marks[j] - marks[i] - marks[k] != 0)

  for i in range(size):
    for j in range(size):
      for k in range(size):
        for l in range(size):
          if i < j and j < k and [i,j] < [k,l]:
            model += (marks[j] - marks[i] != marks[l] - marks[k])

 
  ss = CPM_ortools(model)
  # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
  # ss.ort_solver.parameters.search_branching = ort.AUTOMATIC_SEARCH
  # ss.ort_solver.parameters.cp_model_presolve = False
  ss.ort_solver.parameters.linearization_level = 0
  ss.ort_solver.parameters.cp_model_probing_level = 0
  ss.ort_solver.parameters.num_search_workers = 12
  if ss.solve():
    print("marks:",marks.value())
    print("status:", ss.status())
  print()


size = 8
if len(sys.argv) > 1:
  size = int(sys.argv[1])

golomb(size)
