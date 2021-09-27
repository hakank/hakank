"""
Test of lexical constraint lex_less in cpmpy.


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *



def lex_less_test():

    n = 5
    x = intvar(0,4,shape=n, name="x")
    y = intvar(0,4,shape=n, name="y")    
    
    model = Model (
        lex_less(x,y),
        # lex_greater(x,y),
        x == [0,1,2,3,4]
        )


    print(model)

    ortools_wrapper(model,[x,y])


lex_less_test()


