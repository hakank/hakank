"""
All different with 'explanation' in cpmpy.

This model shows which values that are inconsistent with the all different
constraint. Thus this constraint will never fail.

Compare with 
- http://hakank.org/cpmpy/nvalue.py (and the builtin predicate nvalue/2)
- http://hakank.org/cpmpy/nvalues.py
These are global constraints for counting the number of different values in a list.


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/

"""

import random
from cpmpy import *
import numpy as np
from cpmpy_hakank import *


def print_solution(a):
  x = a[0]
  s = a[1]
  num_conflicts = a[2][0]
  print("x:",x.value(),"s:",s.value(),"num_conflicts:",num_conflicts.value())


def all_different_explain_test(n=4):

    x = intvar(1,n,shape=n,name="x")

    # Where are the conflicts?
    # Shape is len(0.. x.ub), i.e. n+1
    s = boolvar(shape=n+1,name="s") 
    num_conflicts = intvar(0,n,name="num_conflicts")

    model = Model([num_conflicts == sum(s),
                   all_different_explain(x, s),
    
                   # num_conflicts == 0, #  alldifferent (i.e. no duplicates)
                   # num_conflicts <= 1, #  we accept atmost 1 duplicated value
                   ])
  
    ortools_wrapper(model,[x,s,[num_conflicts]],print_solution)

n=4
all_different_explain_test(n)
