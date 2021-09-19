"""
Combinatorial auction in cpmpy.

This is from the Numberjack's tutorial, page 24 (slide 51/175)

Solution:
""
assign:
  obj in {0..100}
  x1 in {0,1}
  x2 in {0,1}
  x3 in {0,1}
  x4 in {0,1}
  x5 in {0,1}
  
subject to:
  Maximise(obj)
  (obj == (((((x1 * 10) + (x2 * 20)) + (x3 * 30)) + (x4 * 40)) + (x5 * 14)))
  (((x1 + x2) + x5) <= 1)
  (((x1 + x3) + x4) <= 1)
  ((x2 + x4) <= 1)
  ((x3 + x4) <= 1)

X  :  0 0 0 1 1
obj: 54
""

For a more general model, see
* combinatorial_auction2.py


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
from cpmpy import *
from cpmpy.solvers import *
import numpy as np
from cpmpy_hakank import *


def combinatorial_auction():

    x1 = intvar(0,1,name='x1')
    x2 = intvar(0,1,name='x2');
    x3 = intvar(0,1,name='x3');
    x4 = intvar(0,1,name='x4');
    x5 = intvar(0,1,name='x5');
    
    obj = intvar(0,100,name='obj')
    model = Model(
        [
        obj == 10*x1 + 20*x2 + 30*x3 + 40*x4 + 14*x5,
        x1 + x2 + x5 <= 1,
        x1 + x3 + x4 <= 1,
        x2 + x4 <= 1,
        x3 + x4 <= 1
        ],
        maximize=obj,
        )
    
    print(model)
    ss = CPM_ortools(model)
    if ss.solve():
        xs = [x1,x2,x3,x4,x5]
        print("X  : ", [x.value() for x in xs])
        print("obj:", obj.value())
        print()
    else:
        print("No solution")

combinatorial_auction()
