"""
Map coloring in cpmpy

From Pascal Van Hentenryck 'The OPL Optimization Programming Language',
page 7, 42.

Symmetry breaking:
* With the simple symmetry breaking constraint that Belgium has color 1
  there are 36 solutions: 
  [1 1 3 2 4 4]
  [1 1 4 2 4 3]
  [1 1 4 3 4 2]
  [1 1 4 3 2 2]
  [1 1 3 4 2 2]
  [1 1 3 4 3 2]
  [1 1 4 2 3 3]
  [1 1 3 2 3 4]
  [1 2 3 4 3 2]
  [1 2 3 4 2 2]
  [1 2 4 3 2 2]
  [1 2 4 3 4 2]
  [1 3 3 4 2 2]
  [1 3 3 4 3 2]
  [1 3 4 2 3 3]
  [1 3 3 2 3 4]
  [1 3 3 2 4 4]
  [1 3 4 2 4 3]
  [1 4 4 2 4 3]
  [1 4 3 2 4 4]
  [1 4 4 3 4 2]
  [1 4 4 3 2 2]
  [1 4 4 2 3 3]
  [1 4 3 2 3 4]
  [1 4 2 3 2 4]
  [1 4 2 3 4 4]
  [1 2 2 3 4 4]
  [1 2 2 3 2 4]
  [1 1 2 3 2 4]
  [1 1 2 3 4 4]
  [1 1 2 4 3 3]
  [1 2 2 4 3 3]
  [1 3 2 4 3 3]
  [1 3 2 4 2 3]
  [1 2 2 4 2 3]
  [1 1 2 4 2 3]
  

* With the added constraint value_precede_chain (that color 1 must be used before
  color 2 which must be used before color 3 etc) there are just 6 solutions:
  [1 2 2 3 4 4]
  [1 2 2 3 2 4]
  [1 2 3 4 2 2]
  [1 2 3 4 3 2]
  [1 1 2 3 2 4]
  [1 1 2 3 4 4]


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def map_coloring(use_value_precede_chain=False):
    print("Use value precede chain symmetry constraint:", use_value_precede_chain)
    
    Belgium     = 0
    Denmark     = 1
    France      = 2
    Germany     = 3
    Netherlands = 4
    Luxembourg  = 5

    countries = [Belgium,Denmark,France,Germany,Netherlands,Luxembourg]

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

    if use_value_precede_chain:
        model += [value_precede_chain(list(range(1,max_num_colors+1)),color)]

    num_solutions = model.solveAll(display=color)
    print("num_solutions:",num_solutions)
                

use_value_precede_chain=False
map_coloring(use_value_precede_chain)

use_value_precede_chain=True
map_coloring(use_value_precede_chain)


