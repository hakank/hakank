"""
Mrs Timkin's Age problem in cpmpy.

From 
http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/index.html
'''
Mrs Timpkin's Age    from 'Amusements in Mathematics, Dudeney', number 43.

When the Timpkinses married eighteen years ago, Timpkins was three
times as old as his wife, and today he is just twice as old as she.
How old is Mrs. Timpkin? 
'''

Answer:
  Mr. Timpkin age: 72
  Mrs Timpkin age: 36

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *




def timpkin(married_years_ago_fixed=0):
    
  model = Model()


  # variables
  # At least 18 years old
  t = intvar(18,100,name="Mr. Timpkin age")
  w = intvar(18,100,name="Mrs Timpkin age")
  
  # This could - of course - be a constant (18)
  # but it might interesting/funny/instructive 
  # to also let it be a decision variable.
  # Note that this will give non-legal (in legal terms)
  # marriages if we don't restrict the domains above.
  married_years_ago = intvar(0,100,name="Married years ago")
  if married_years_ago_fixed > 0:
    model += (married_years_ago == married_years_ago_fixed)
    
  model += (t - married_years_ago == 3 * (w - married_years_ago))
  model += (t == 2*w)

  def print_sol():
    print("t:",t.value(), "w:",w.value())

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)  


print("The stated puzzle:")
timpkin(18)
print()
print("No fixed marriage-when-ago age:")
timpkin()
