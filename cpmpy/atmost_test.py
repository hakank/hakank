"""
Test of atmost, atleast, and exactly in cpmpy.


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def atmost_test(n=4):
    
    x = intvar(0,n,shape=n,name="x")

    model = Model (
        [atmost(x,i,2) for i in range(n)]
        # [atleast(x,0,2),atleast(x,1,2)]
        # [exactly(x,0,1),exactly(x,1,2)]
        )

    print("model:",model)
    ortools_wrapper(model,[x])

atmost_test(4)

