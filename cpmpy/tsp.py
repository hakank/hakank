"""
Traveling Salesman Problem, integer programming model in cpmpy.

From GLPK:s example tsp.mod
'''
TSP, Traveling Salesman Problem

Written in GNU MathProg by Andrew Makhorin <mao@mai2.rcnet.ru> */

The Traveling Salesman Problem (TSP) is stated as follows.
Let a directed graph G = (V, E) be given, where V = {1, ..., n} is
a set of nodes, E <= V x V is a set of arcs. Let also each arc
e = (i,j) be assigned a number c[i,j], which is the length of the
arc e. The problem is to find a closed path of minimal length going
through each node of G exactly once.
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *




def tsp(n,num_edges,E,c):

  # x[i,j] = 1 means that the salesman goes from node i to node j 
  x = boolvar(shape=num_edges,name="x")

  # y[i,j] is the number of cars, which the salesman has after leaving
  # node i and before entering node j; in terms of the network analysis,
  # y[i,j] is a flow through arc (i,j) 
  y = intvar(0,n,shape=num_edges,name="y")

  # the objective is to make the path length as small as possible 
  total = intvar(0,9999,name="total")

  model = Model(minimize=total)

  #
  # constraints
  #
  model += (total == sum([(c[i] * x[i]) for i in range(num_edges)]))

  # the salesman leaves each node i exactly once 
  for i in range(n):
    model += (sum([x[k] for k in range(num_edges) if E[k][0] == i]) == 1)

  # the salesman enters each node j exactly once 
  for j in range(n):
    model += (sum([x[k] for k in range(num_edges) if E[k][1] == j]) == 1)


  # """
  # Constraints above are not sufficient to describe valid tours, so we
  # need to add constraints to eliminate subtours, i.e. tours which have
  # disconnected components. Although there are many known ways to do
  # that, I invented yet another way. The general idea is the following.
  # Let the salesman sells, say, cars, starting the travel from node 1,
  # where he has n cars. If we require the salesman to sell exactly one
  # car in each node, he will need to go through all nodes to satisfy
  # this requirement, thus, all subtours will be eliminated. 
  # 

  # if arc (i,j) does not belong to the salesman's tour, its capacity
  # must be zero; it is obvious that on leaving a node, it is sufficient
  # to have not more than n-1 cars 
  # """
  for k in range(num_edges):
    model += (y[k] >= 0)
    model += (y[k] <= (n-1) * x[k])

  # node[i] is a conservation constraint for node i 
  for i in range(n):
    # summary flow into node i through all ingoing arcs 
    model += ( (
                 sum([y[k] for k in range(num_edges) if E[k][1] == i])
                 # plus n cars which the salesman has at starting node 
                 + (n if i == 0 else 0)
                 )
               == # must be equal to 
                  # summary flow from node i through all outgoing arcs 
               (
                sum([y[k] for k in range(num_edges) if E[k][0] == i])
                # plus one car which the salesman sells at node i 
                + 1)
              )

  ss = CPM_ortools(model)
  # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
  ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
  # ss.ort_solver.parameters.cp_model_presolve = False
  # ss.ort_solver.parameters.linearization_level = 0
  ss.ort_solver.parameters.cp_model_probing_level = 0

  num_solutions = 0
  if ss.solve():
    num_solutions += 1
    print("total:",total.value())
    print("x:",x.value())
    print("visited:",[i for i in range(num_edges) if x[i].value() == 1])
    print("y:",y.value())    
    
  print()
  print("num_solutions:", num_solutions)  


# 
# data
# 

# """
# These data correspond to the symmetric instance ulysses16 from:
# Reinelt, G.: TSPLIB - A travelling salesman problem library.
# ORSA-Journal of the Computing 3 (1991) 376-84;
# http://elib.zib.de/pub/Packages/mp-testdata/tsp/tsplib 
# 
# The optimal solution is 6859
# """
n = 16
num_edges = 240

# This is a matrix of (n-1) x (n-1) which excludes the main diagonal (i,i)
E = [ [i,j] for i in range(n) for j in range(n) if i != j ]

# for i in range(num_edges):
#   print(E[i])

c=[
  509,501,312,1019,736,656,60,1039,726,2314,479,448,479,619,150,509,126,474,1526,
  1226,1133,532,1449,1122,2789,958,941,978,1127,542,501,126,541,1516,1184,1084,536,
  1371,1045,2728,913,904,946,1115,499,312,474,541,1157,980,919,271,1333,1029,2553,
  751,704,720,783,455,1019,1526,1516,1157,478,583,996,858,855,1504,677,651,600,401,
  1033,736,1226,1184,980,478,115,740,470,379,1581,271,289,261,308,687,656,1133,1084,
  919,583,115,667,455,288,1661,177,216,207,343,592,60,532,536,271,996,740,667,1066,759,
  2320,493,454,479,598,206,1039,1449,1371,1333,858,470,455,1066,328,1387,591,650,656,
  776,933,726,1122,1045,1029,855,379,288,759,328,1697,333,400,427,622,610,2314,2789,
  2728,2553,1504,1581,1661,2320,1387,1697,1838,1868,1841,1789,2248,479,958,913,
  751,677,271,177,493,591,333,1838,68,105,336,417,448,941,904,704,651,289,216,454,
  650,400,1868,68,52,287,406,479,978,946,720,600,261,207,479,656,427,1841,105,52,
  237,449,619,1127,1115,783,401,308,343,598,776,622,1789,336,287,237,636,150,542,499,
  455,1033,687,592,206,933,610,2248,417,406,449,636
  ]

tsp(n,num_edges,E,c)
