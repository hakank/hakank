"""
Test of chain constraint in cpmpy

  x = intvar(1,3,shape=3)
  chain(>=,x)
  ->  [[1,1,1],[2,1,1],[2,2,1],[2,2,2],[3,1,1],[3,2,1],[3,2,2],[3,3,1],[3,3,2],[3,3,3]]

However, we must use the operator package, so the call must be
  x = intvar(1,3,shape=3)
  chain(optator.gt,x)

Many of the binary operator.* has a definition already, e.g.
(from cpmpy_hakank.py):
increasing, increasing_strict, decreasing, descreasing_strict
and
AllDifferent, AllEqual.

See below for a more intresting example using the Infix hack
from https://code.activestate.com/recipes/384122/ .

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *
import operator


# From https://code.activestate.com/recipes/384122/
class Infix:
    def __init__(self, function):
        self.function = function
    def __ror__(self, other):
        return Infix(lambda x, self=self, other=other: self.function(other, x))
    def __or__(self, other):
        return self.function(other)
    def __rlshift__(self, other):
        return Infix(lambda x, self=self, other=other: self.function(other, x))
    def __rshift__(self, other):
        return self.function(other)
    def __call__(self, value1, value2):
        return self.function(value1, value2)


def chain_test(n):

  # variables
  x = intvar(1,n,shape=n,name="x")
  
  # constraints
  model = Model([# chain(operator.lt,x), # increasing_strict(x)
                 # chain(operator.le,x), # increasing(x)
                 # chain(operator.ne,x), # AllDifferent
                 # chain(operator.eq,x), # AllEqual
                 # chain(operator.ge,x), # decreasing
                 # chain(operator.gt,x), # decreasing_strict
                 # chain(operator.ge,x), # decreasing(x)
    
                 # It's perhaps only interesting on these kind
                 # of cases:
                 # chain(Infix(lambda a,b: 2*a>=b),x),
                 chain(Infix(lambda a,b: 2*a != b),x),                 
                 ])
  
  print(model)

  num_solutions = model.solveAll(display=x)
  print("num_solutions:",num_solutions)
  

n = 5
chain_test(n)


