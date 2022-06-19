"""
Pairs divides the sum puzzle in cpmpy.

From comp.lang.prolog
'''
Date: Sat, Feb 28 2009 3:55 am
From: Nick Wedd

Here is a puzzle which I found surprisingly easy to program Prolog to
generate solutions to.  If any of you teach Prolog to students, you
might use it as an example (like the goat-wolf-cabbage thing).

Find a set of four distinct positive integers such that, for every pair
of them, their difference divides their sum.

Find lots of such sets.

As above, but sets of five distinct positive integers.

As above, but sets of six ...
'''


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys,math
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def pair_divides_the_sum(n=4,max_val=100):
    
    x = intvar(1,max_val,shape=n,name="x")
    z = intvar(1,max_val*n,name="z")

    model = Model([AllDifferent(x),
                   z == sum(x),
                   increasing_strict(x),
                   z % n == 0
                   ])

    t_counter = 0
    for i in range(n):
        for j in range(i+1,n):
            # This don't work, probably since the
            # domain includes 0 which is not allowed
            # model += [z % abs(x[i]-x[j]) == 0]
            
            # This works, however:
            t = intvar(1,max_val,name=f"t_{t_counter}")
            t_counter += 1
            model += [t == abs(x[i]-x[j]),
                      z % t == 0]

    print(model)
    # following 5 lines are advanced cpmpy debugging things...
    from cpmpy.transformations.flatten_model import flatten_constraint, flatten_model
    from cpmpy.transformations.get_variables import print_variables
    mf = flatten_model(model)
    print_variables(mf)
    print(mf)

    def print_sol():
        print("x:",x.value(),"z:",z.value())

    ss = CPM_ortools(model)
    ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0    
    num_solutions = ss.solveAll(display=print_sol)
    print("num_solutions:",num_solutions)


import cpmpy
print("cpmpy version:", cpmpy.__version__)
n = 4
pair_divides_the_sum(n)
