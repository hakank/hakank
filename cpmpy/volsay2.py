"""
Volsay problem in cpmpy.

From the OPL model volsay.mod
Using arrays.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def volsay2():


  
  # data
  num_products = 2
  Gas = 0
  Chloride = 1
  products = ['Gas', 'Chloride']

  # declare variables
  production = intvar(0,100000,shape=num_products,name="production")
  obj = 40 * production[Gas] + 50 * production[Chloride]

  model = Model([production[Gas] + production[Chloride] <= 50,
                 3 * production[Gas] + 4 * production[Chloride] <= 180
                 ],
                maximize=obj)

  ss = CPM_ortools(model)
  num_solutions = 0
  if ss.solve():
    num_solutions += 1
    print("obj:",obj.value())
    print("production:",production.value())
    print("Gas prod:", production[Gas].value())
    print("Chloride prod:", production[Chloride].value())

  # print("num_solutions:", num_solutions)

volsay2()


