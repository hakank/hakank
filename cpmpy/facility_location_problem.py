"""
Facility location problem in cpmpy.

From 
http://www.math.ohiou.edu/~vardges/math443/homeworks/homework1.doc
'''
Problem 1. Facility Location problem.

A company is considering opening warehouses in 
  four cities: New York, Los Angeles, Chicago, and Atlanta. 
Each warehouse can ship 100 units per week. 
The weekly fixed cost of keeping each warehouse open is 
  $400 for New York, $500 for Los Angeles, $300 for Chicago, 
  and $150 for Atlanta. 

Region 1 of the country requires 80 units per week, 
region 2 requires 70 units per week, and 
region 3 requires 40 units per week. 

The costs of sending 1 unit from a warehouse to a region 
are shown in the following table:
              To
from          Region 1  Region 2   Region 3
New York      $20       $40        $50
Los Angeles   $48       $15        $26
Chicago       $26       $35        $18
Atlanta       $24       $50        $35

We wish to meet weekly demands at minimum cost, subject to the preceding information 
and the following restrictions:

 1. If the New York warehouse is opened, then the Los Angeles warehouse 
    must be opened.
 2. At most three warehouses can be opened.
 3. Either the Atlanta or the Los Angeles warehouse must be opened.

Formulate an integer program that can be used to minimize the weekly costs 
of meeting demands. 
'''

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *

def facility_location():

    num_companies = 4
    num_regions = 3
    max_shipping = 100 # max shipping units per week per warehouse

    new_york    = 0
    los_angeles = 1
    chicago     = 2
    atlanta     = 3
    warehouse_s = ["New York","Los Angeles","Chicago","Atlanta"]
    fixed_cost = [400,500,300,150] # dollars per week
    demands = [80,70,40] # units per week
    costs = cpm_array([[20, 40, 50],
                       [48, 15, 26],
                       [26, 35, 18],
                       [24, 50, 35]])

    #  is this warehouse open?
    open_warehouse = boolvar(shape=num_companies,name="open_warehouse")

    # what to ship to each region
    ships = intvar(0,max_shipping,shape=(num_companies,num_regions),name="ships")

    # total cost
    total_cost = intvar(0,10000,name="total_cost")
    
    # constraints
    model = Model()
  
    # each warehouse can only send max 100 units per week
    for i in range(num_companies):
        model += [sum([open_warehouse[i]*ships[i,j] for j in range(num_regions)]) <= max_shipping]

    # the demands of the regions
    for j in range(num_regions):
        model += [sum([open_warehouse[i]*ships[i,j] for i in range(num_companies)]) >= demands[j]]

    #  connect open with ships
    for i in range(num_companies):
        model += [ open_warehouse[i].implies(sum([ships[i,j] for j in range(num_regions)])  > 0)]

    #  total cost
    model += [total_cost == sum([open_warehouse[i]*(fixed_cost[i] +
                                                    sum([ships[i,j]*costs[i,j]
                                                         for j in range(num_regions)]))
                                 for i in range(num_companies)])]

    #  1. If the New York warehouse is opened, then the Los Angeles warehouse 
    #     must be opened.
    model += [open_warehouse[new_york].implies(open_warehouse[los_angeles])]

    #  2. At most three warehouses can be opened.
    model += [sum(open_warehouse) <= 3]
  
    #   3. Either the Atlanta or the Los Angeles warehouse must be opened.
    model += [open_warehouse[atlanta] | open_warehouse[los_angeles]]
     
    model.minimize(total_cost)

    ss = CPM_ortools(model)
    if ss.solve():
      print("total_cost:", total_cost.value())
      print("open_warehouse:", open_warehouse.value())
      print("ships:\n", ships.value())
      

facility_location()
