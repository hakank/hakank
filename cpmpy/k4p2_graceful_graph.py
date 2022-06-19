"""
K4P2 Graceful Graph in cpmpy.

http://www.csplib.org/Problems/prob053/
'''
Proposed by Karen Petrie
A labelling f of the nodes of a graph with q edges is graceful if f assigns each node a unique label
from 0,1,...,q and when each edge xy is labelled with |f(x)-f(y)|, the edge labels are all different.
Gallian surveys graceful graphs, i.e. graphs with a graceful labelling, and lists the graphs whose status
is known.

[ picture ]

All-Interval Series is a special case of a graceful graph where the graph is a line.
'''

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/
  
"""
from cpmpy import *
import cpmpy.solvers
import numpy as np
from cpmpy_hakank import *


def k4p2gracefulgraph2():

  model = Model()

  # data
  m = 16
  n = 8

  # Note: 1 based. Adjusted below
  graph = [[1, 2],
           [1, 3],
           [1, 4],
           [2, 3],
           [2, 4],
           [3, 4],
           
           [5, 6],
           [5, 7],
           [5, 8],
           [6, 7],
           [6, 8],
           [7, 8],
           
           [1, 5],
           [2, 6],
           [3, 7],
           [4, 8]]

  # variables
  nodes = intvar(0,m,shape=n,name="nodes")
  edges = intvar(1,m,shape=m,name="edges")

  # constraints
  for i in range(m):
    # also, adjust for 1-based
    model += (abs(nodes[graph[i][0]-1] - nodes[graph[i][1]-1]) == edges[i])

  model += (AllDifferent(edges))
  model += (AllDifferent(nodes))

  print(model)

  def print_sol():
    print("edges:", edges.value())
    print("nodes:", nodes.value())
    print()

  ss = CPM_ortools(model)
  ss.ort_solver.parameters.linearization_level = 0
  ss.ort_solver.parameters.cp_model_probing_level = 0
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:",num_solutions)

k4p2gracefulgraph2()
  
