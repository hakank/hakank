"""
SONET problem in cpmpy.

Translation of the ESSENCE's model in the Minion Translator examples:
http://www.cs.st-andrews.ac.uk/~andrea/examples/sonet/sonet_problem.eprime
'''
The SONET problem is a network design problem: set up a network between
n nodes, where only certain nodes require a connection.
Nodes are connected by putting them on a ring, where all nodes
on a ring can communicate. Putting a node on a ring requires a so-called
ADM, and each ring has a capacity of nodes, i.e. ADMs. There is a certain 
amount of rings, r, that is available. The objective is to set up a network
by using a minimal amount of ADMs.


About the problem model

The problem model has the amount of rings ('r'), amount of nodes('n'),
the 'demand' (which nodes require communication) and node-capacity of each 
ring ('capacity_nodes') as parameters.
The assignement of nodes to rings is modelled by a 2-dimensional matrix 'rings',
indexed by the amnount of rings and nodes. The matrix-domain is boolean:
If the node in column j is assigned to the ring in row i, then rings[i,j] = 1 
and 0 otherwise. So all the '1's in the matrix 'rings' stand for an ADM.
Hence the objective is to minimise the sum over all columns and rows of matrix
'rings'.
'''


This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
import math, sys, os
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *

 

def sonet_problem(r,n,demand,capacity_nodes):


  # variables
  rings = boolvar(shape=(r,n),name="rings")
  z = intvar(0,sum(capacity_nodes),name="z")

  model = Model(z == rings.sum())

  # """
  # capacity of each ring must not be exceeded     
  # """
  for ring in range(r):
    model += [sum([rings[ring, client] for client in range(n)]) <= capacity_nodes[ring]]

  # """
  # if there is a demand between 2 nodes, then there has to exist 
  # a ring, on which they are both installed
  # """
  for client1 in range(n):
    for client2 in range(n):
      if client1 < client2 and demand[client1][client2] == 1:
        model += [sum([rings[ring,client1] + rings[ring,client2] >= 2 for ring in range(r)])>= 1]

  model.minimize(z)


  ss = CPM_ortools(model)
  # ss.ort_solver.parameters.log_search_progress = True
  # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
  ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
  # ss.ort_solver.parameters.cp_model_presolve = False
  ss.ort_solver.parameters.linearization_level = 0
  # ss.ort_solver.parameters.cp_model_probing_level = 0

  num_solutions = 0
  if ss.solve():
    num_solutions += 1

    print("z:",z.value())
    print("rings:")
    print(rings.value())
    print(flush=True)

  print("num_solutions:", num_solutions)
  print("Num conflicts:", ss.ort_solver.NumConflicts())
  print("NumBranches:", ss.ort_solver.NumBranches())
  print("WallTime:", ss.ort_solver.WallTime())
  
  print()


r = 4  # upper bound for amount of rings
n = 5  # amount of clients

"""
we have double entries here because of the symmetric structure!
"""
demand = [[0,1,0,1,0],
          [1,0,1,0,0],
          [0,1,0,0,1],
          [1,0,0,0,0],
          [0,0,1,0,0]
          ]
  
capacity_nodes = [3,2,2,1]

sonet_problem(r,n,demand,capacity_nodes)
