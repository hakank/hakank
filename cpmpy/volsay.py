"""
Volsay problem in cpmpy.

From the OPL model volsay.mod


This CPMpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my CPMpy page: http://hakank.org/cpmpy/

"""
import math, sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import CPM_ortools
from cpmpy_hakank import *
import sys


def volsay():

  # declare variables
  Gas = intvar(0, 100000, name="Gas")
  Chloride = intvar(0, 100000, name="Cloride")
  obj = intvar(0,100000,name="obj")

  #
  # constraints
  #
  model = Model([Gas + Chloride <= 50,
                 3 * Gas + 4 * Chloride <= 180,
                 obj == 40 * Gas + 50 * Chloride
                 ],
                maximize=obj)


  ss = CPM_ortools(model)
  if ss.solve():
      print()
      print('objective = ', obj.value())
      print('Gas = ', Gas.value())
      print('Chloride:', Chloride.value())
      print()

volsay()

