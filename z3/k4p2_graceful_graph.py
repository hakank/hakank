#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# K4P2 Graceful Graph in Z3
#
# http://www.csplib.org/Problems/prob053/
# """
# Proposed by Karen Petrie
# A labelling f of the nodes of a graph with q edges is graceful if f assigns each node a unique label
# from 0,1,...,q and when each edge xy is labelled with |f(x)-f(y)|, the edge labels are all different.
# Gallian surveys graceful graphs, i.e. graphs with a graceful labelling, and lists the graphs whose status
# is known.
# 
# [ picture ]
# 
# All-Interval Series is a special case of a graceful graph where the graph is a line.
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()


# data
m = 16
n = 8

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
nodes = makeIntVector(sol,"nodes",n,0,m)
edges = makeIntVector(sol,"edges",m,0,m-1)

# constraints
for i in range(m):
    sol.add(Abs(nodes[graph[i][0]-1] - nodes[graph[i][1]-1]) == edges[i])

sol.add(Distinct(edges))
sol.add(Distinct(nodes))



num_solutions = 0
while sol.check() == sat:
  num_solutions += 1
  mod = sol.model()
  print "edges:", [mod.eval(edges[i]) for i in range(m)]
  print "nodes:", [mod.eval(nodes[i]) for i in range(n)]
  print
  getDifferentSolution(sol,mod,edges,nodes)

print "num_solutions:", num_solutions  
