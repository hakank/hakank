"""
Volsay problem in cpmpy.

From the OPL model volsay.mod
'''
Consider a Belgian company Volsay, which specializes in producing ammoniac gas 
(NH3) and ammonium chloride (NH4Cl). Volsay has at its disposal 50 units of 
nitrogen (N), 180 units of hydrogen (H), and 40 units of chlorine (Cl). The company 
makes a profit of 40 Euros for each sale of an ammoniac gas unit and 50 Euros 
for each sale of an ammonium chloride unit. Volsay would like a production plan 
maximizing its profits given its available stocks. 
'''

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *



def volsay3(obj_value=None):

  # data
  num_products = 2
  Gas, Chloride = range(num_products)
  products = ['Gas', 'Chloride']
  
  num_components = 3
  Nitrogen, Hydrogen, Chlorine = range(num_components)
  components = ['Nitrogen', 'Hydrogen', 'Chlorine']

  # Demands of Component per Product
  Demand = [[1, 3, 0],
            [1, 4, 1]] 

  Profit = [30, 40]      # per product
  Stock  = [50, 180, 40] # per component


  # declare variables
  Production = intvar(0,10000,shape=num_products)
  obj = intvar(0, 10000,name="obj")

  if obj_value == None:
    model = Model(maximize=obj)
  else:
    model = Model(obj==obj_value)

  #
  # constraints
  #
  for c in range(num_components):
    model += (sum([Demand[p][c]* Production[p] for p in range(num_products)]) <= Stock[c])

  # objective to minimize
  model += (obj == sum([Profit[p]*Production[p] for p in range(num_products)]))

  def print_sol():
      print("obj:", obj.value())
      for i in range(num_products):
        print(products[i], '=', Production[i].value())
      print()
    

  ss = CPM_ortools(model)
  if obj_value == None:
    if ss.solve():
      print_sol()
      return obj.value()
  else:
    num_solutions = ss.solveAll(display=print_sol)      
    print("num_solutions:",num_solutions)

obj = volsay3(None)
print("\nAll obtimal solutions:")
volsay3(obj)
