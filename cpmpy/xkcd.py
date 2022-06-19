"""
xkcd problem in cpmpy.

See http://xkcd.com/287/

Some amount (or none) of each dish should be ordered to give a total 
of exact 15.05

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/

"""
from cpmpy import *
import numpy as np
from cpmpy_hakank import *

def xkcd(price,z):
    x = intvar(0,100,shape=len(price),name="x")    
    model = Model(
        z == sum(x* price)
        )
    print("model:\n", model)

    model.solveAll(display=x)


price = [215, 275, 335, 355, 420, 580]
z = 1505
xkcd(price,z)

