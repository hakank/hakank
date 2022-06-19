"""
Huey, Dewey and Louie problem in cpmpy.

From Marriott & Stucket, Programming with Constraints, page 42
'''
Huey, Dewey and Louie are being questioned by their uncle. These are the 
statements the make:
  Huey: Dewey and Louie has equal share in it; if one is quitly, so is the other.
  Dewey: If Huey is guilty, then so am I.
  Louie: Dewey and I are not both quilty.

Their uncle, knowing that they are cub scouts, realises that they cannot tell a lie.
Has he got sufficient information to decide who (if any) are quilty?
'''


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def huey_dewey_louie():

  p = boolvar(shape=3)
  huey, dewey, louie = p
  model = Model([
    # Huey: Dewey and Louie has equal share in it; if one is quitly, so is the other.
    dewey == louie,

    # Dewey: If Huey is guilty, then so am I.
    huey.implies(dewey),
    
    # Louie: Dewey and I are not both quilty.
    ~(dewey & louie)
    ])

  def print_sol():
    print("huey:", huey.value() )
    print("dewey:", dewey.value() )
    print("louie:", louie.value() )        
    
  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)


huey_dewey_louie()
