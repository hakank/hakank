"""
Autoref problem in cpmpy.

From Global constraint catalog
http://www.emn.fr/z-info/sdemasse/gccat/Kautoref.html
'''
A constraint that allows for modelling the autoref problem with one single constraint. 
The autoref problem is a generalisation of the problem of finding a magic serie 
and can be defined in the following way. Given an integer n > 0 and an integer 
m >= 0, the problem is to find a non-empty finite series S=(s0,s1,...,sn,sn+1) 
such that (1) there are si occurrences of i in S for each integer i ranging 
from 0 to n, and (2) sn+1=m. This leads to the following model:

global_cardinality(
 <var-s0,var-s1,...,var-sn,var-m>,
 val-0 noccurrence-s0
 val-1 noccurrence-s1,
 < ... >
 val-n noccurrence-sn
)

23, 2, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 5 
and 
23, 3, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 5 
are the two unique solutions for n=27 and m=5.
'''

Cf magic_sequence.py for a similar (but not identical) problem.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

def autoref(s):
  """
  autoref(s)

  Ensure that the number of occurrences of i in s is s[i].
  s should be an array of 0..n+1
  """
  return [global_cardinality_count(s,s)]

def autoref_model(n=27,m=5):
  print("n:",n,"m:",m)

  # variables
  s = intvar(0,n,shape=n+2,name="s")

  # constraints
  model = Model(s[n+1]==m)
  
  # for i in range(n+1):
  #   model += (s[i] == sum([s[j] == i for j in range(n+2)]))
  # Simpler:
  # model += (global_cardinality_count(s,s))
  # As a constraint:
  model += (autoref(s))

  def print_sol():
    print("s:",[v for v in s.value()])    

  ss = CPM_ortools(model)
  # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
  # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
  # ss.ort_solver.parameters.cp_model_presolve = False
  # ss.ort_solver.parameters.linearization_level = 0
  # ss.ort_solver.parameters.cp_model_probing_level = 0
  
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)
  print()

n = 27
m = 5                
autoref_model(n,m)

autoref_model(42,17)
