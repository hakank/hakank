"""
Map coloring in cpmpy

From Pascal Van Hentenryck "The OPL Optimization Programming Language",
page 7, 42.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def map_coloring():
    Belgium     = 0
    Denmark     = 1
    France      = 2
    Germany     = 3
    Netherlands = 4
    Luxembourg  = 5

    num_countries = 6
    max_num_colors = 4
    
    color = intvar(1,max_num_colors,shape=num_countries,name="color")

    model = Model(
         color[Belgium] == 1, # Symmetry breaking
         color[France] != color[Belgium],
         color[France] != color[Luxembourg],
         color[France] != color[Germany],
         color[Luxembourg] != color[Germany],
         color[Luxembourg] != color[Belgium],
         color[Belgium] != color[Netherlands],
         color[Belgium] != color[Germany],
         color[Germany] != color[Netherlands],
         color[Germany] != color[Denmark]
        )

    ortools_wrapper(model,[color])
                

map_coloring()

