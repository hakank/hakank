"""
Constraint all_different_reif (reified) in cpmpy.

all_different_reif(x,b)

b is 1 if all values in x are different, else 0.

For this we use a decomposition of all_different that
uses nvalue.

Here we also show the variant were a variable (b) is returned
from a constraints. Note that this requires that the model is
an argument to the method.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys,math
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *
from itertools import combinations


def print_solution(a):
    x = a[0]
    b = a[1][0]
    print("x:",x.value(), "b:",b.value(), "all different!" if b.value() else "")



def all_different_reif_test(n):

    x = intvar(1,n,shape=n,name="x")
    b = boolvar(name="b")

    model = Model([all_different_reif(x,b),
                   increasing(x)
                   # b == 1 # Force all to be different
                   ])

    ortools_wrapper2(model,[x,[b]],print_solution)


#
# Here we returns b instead from the constraint.
#
def all_different_reif_test2(n):

    x = intvar(1,n,shape=n,name="x")
    b = boolvar(name="b")

    model = Model([increasing(x)])

    model += [b == all_different_reif_m(model,x),
             # b == 1 # Force all to be different
             ]

    ortools_wrapper2(model,[x,[b]],print_solution)



n = 4
all_different_reif_test(n)

print("\nSecond method:")
all_different_reif_test2(n)
