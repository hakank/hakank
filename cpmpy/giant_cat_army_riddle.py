"""
Giant Cat Army riddle in cpmpy.
Via https://stackoverflow.com/questions/65511714/about-building-a-list-until-it-meets-conditions
'''
Basically you start with [0], then you build this list by using one of three 
operations: adding 5, adding 7, or taking sqrt. You successfully complete the 
game when you have managed to build a list such that 2,10 and 14 appear 
on the list, in that order, and there can be other numbers between them.

The rules also require that all the elements are distinct, they're all <=60 
and are all only integers. For example, starting with [0], you can
apply (add5, add7, add5), which would result in [0, 5, 12, 17], but since 
it doesn't have 2,10,14 in that order it doesn't satisfy the game.
'''

There are 99 optimal solutions of length 24. Here is one solution:
  x: [0, 5, 12, 19, 26, 31, 36, 6, 11, 16, 4, 2, 9, 3, 10, 15, 20, 27, 32, 37, 42, 49, 7, 14]
  With the operations:
    0 (+5) 5 (+7) 12 (+7) 19 (+7) 26 (+5) 31 (+5) 36 (//2) 6 (+5) 11 (+5) 
    16 (//2) 4 (//2) 2 (+7) 9 (//2) 3 (+7) 10 (+5) 15 (+5) 20 (+7) 27 (+5) 
    32 (+5) 37 (+5) 42 (+7) 49 (//2) 7 (+7) 14 



Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *


def giant_cat_army_riddle(n=60):

  model = Model()

  maxval = 60

  x = intvar(0,maxval,shape=n,name="x")
  
  model = Model([AllDifferent(x)])

  # Given and symmetry breaking
  model += [x[0] == 0]

  # x[1] == 5 || x[1] == 7
  model += [(x[1] == 5) | (x[1] == 7)]
  model += (x[n-1] == 14)

  # Encode x[i] == x[i]**2 (i.e. the sqrt operation)
  # x2 = [model.NewIntVar(0,maxval**2,f"x2[{i}]") for i in range(maxval)]
  # for i in range(n):
  #  model.AddMultiplicationEquality(x2[i],[x[i],x[i]])
  
  for i in range(n-1):
      model += [(x[i+1] == x[i] + 5) |
                (x[i+1] == x[i] + 7) |
                (x[i] == x[i+1]*x[i+1])]
      
  # 0 .. 2 .. 10 .. 14
  ix2 = intvar(1,n,name="ix2")
  ix10 = intvar(1,n,name="ix10")            
  model += [2 == x[ix2]]
  model += [10 == x[ix10]]            
  model += [ix2 < ix10]

  def print_sol():
    print("n:",n)
    xval = x.value()
    print("x:",xval)
    print("With the operations:")
    for i in range(n):
      if i > 0:
        if xval[i]-xval[i-1] == 5:
          print("(+5)", end=" ")
        elif xval[i]-xval[i-1] == 7:
          print("(+7)", end=" ")
        else: 
          print("(sqrt)", end=" ")
      print(xval[i],end=" ")
    print()

  ss = CPM_ortools(model)
  # ss.ort_solver.parameters.search_branching = cp.PORTFOLIO_SEARCH
  # ss.ort_solver.parameters.cp_model_presolve = False # Slightly faster
  # ss.ort_solver.parameters.linearization_level = 0
  # ss.ort_solver.parameters.cp_model_probing_level = 0
  
  num_solutions = ss.solveAll(solution_limit=1,display=print_sol)
  return num_solutions


# Try different sequence lengths
for n in range(5,60):
  sol = giant_cat_army_riddle(n)
  if sol != 0:
    min_len = n 
    break
print("\nmin_len:",min_len)
