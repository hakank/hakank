"""
Warehouse location in cpmpy.

From OPL model warehouse.mod
Solution:
'''
Optimal solution found with objective: 383
open= [1 1 1 0 1]
storesof= [{3} {1 5 6 8} {7 9} {} {0 2 4}]
'''

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *

def warehouse_location():

  # data
  fixed = 30
  warehouses = ["Bonn", "Bordeaux", "London", "Paris", "Rome"]
  num_warehouses = len(warehouses)
  num_stores = 10
  capacity = [1,4,2,1,3]
  supply_cost =  np.array([[ 20, 24, 11, 25, 30 ], 
                          [ 28, 27, 82, 83, 74 ],
                          [ 74, 97, 71, 96, 70 ],
                          [  2, 55, 73, 69, 61 ],
                          [ 46, 96, 59, 83,  4 ],
                          [ 42, 22, 29, 67, 59 ],
                          [  1,  5, 73, 59, 56 ],
                          [ 10, 73, 13, 43, 96 ],
                          [ 93, 35, 63, 85, 46 ],
                          [ 47, 65, 55, 71, 95 ] 
                          ])

  # variables
  open_warehouse = boolvar(shape=num_warehouses,name="open_warehouse")
  supply = boolvar(shape=(num_stores, num_warehouses),name="supply")
  z = intvar(0,1000,name="z")

  # constraints
  model = Model()

  model += [z == sum([fixed * open_warehouse[w] for w in range(num_warehouses)]) +
            sum([ supply_cost[s][w] * supply[s, w] for w in range(num_warehouses) for s in range(num_stores)])
            ]

  for s in range(num_stores):
    model += [sum(supply[s]) == 1]
            

  for w in range(num_warehouses):
    for s in range(num_stores):
      model += [supply[s, w] <= open_warehouse[w]]

  for w in range(num_warehouses):
    model += [sum([supply[s, w]for s in range(num_stores)]) <= capacity[w]  ]

  model.minimize(z)

  ss = CPM_ortools(model)
  if ss.solve():
    print("z:", z.value())
    open_res = open_warehouse.value()
    print("open_warehouse:", open_res*1)
    supply_res = supply.value()*1
    print("supply:")
    print(supply_res)    
    for w in range(num_warehouses):
      if open_res[w]:
        print(warehouses[w],": stores :", [s for s in range(num_stores) if supply_res[s,w] == 1])          
      else:
        print(warehouses[w],": not open")
    
  print()

warehouse_location()
