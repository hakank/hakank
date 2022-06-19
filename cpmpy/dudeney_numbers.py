"""
Dudeney numbers in cpmpy.

This is a cpmpy version of Pierre Schaus Google CP Solver
(or-tools/Python) model Dudeney number from his blog post
http://cp-is-fun.blogspot.com/2010/09/test-python.html
""  
I discovered yesterday Dudeney Numbers
A Dudeney Numbers is a positive integer that is a perfect cube such
that the sum of its decimal digits is equal to the cube root of the
number. There are only six Dudeney Numbers and those are very easy to
find with CP.

I made my first experience with google cp solver so find these numbers
(model below) and must say that I found it very convenient to build
CP models in python!

When you take a close look at the line:
solver.Add(sum([10**(n-i-1)*x[i] for i in range(n)]) == nb)
It is difficult to argue that it is very far from dedicated
optimization languages!
""

Also see http://en.wikipedia.org/wiki/Dudeney_number


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

def dudeney(n=6):
    x  = intvar(0, 9,shape=n,name="x")
    nb = intvar(1,10**n-1, name="nb")
    s  = intvar(1,9*n,name="s")
    
    model = Model (
        nb == s*s*s,
        to_num(x,nb,10),
        sum(x) == s,
        )

    def print_sol():
        print('x:',x.value())
        print('nb:',nb.value())
        print('s:', s.value())
        print()

    ss = CPM_ortools(model)
    ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0
    ss.solveAll(display=print_sol)


n = 6
dudeney(n)



