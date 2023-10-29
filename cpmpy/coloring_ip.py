"""
Graph Coloring Problem (integer programming) in cpmpy.

Inspired by the GLPK:s model color.mod
'''
COLOR, Graph Coloring Problem

Written in GNU MathProg by Andrew Makhorin <mao@mai2.rcnet.ru>

Given an undirected loopless graph G = (V, E), where V is a set of
nodes, E <= V x V is a set of arcs, the Graph Coloring Problem is to
find a mapping (coloring) F: V -> C, where C = {1, 2, ... } is a set
of colors whose cardinality is as small as possible, such that
F(i) != F(j) for every arc (i,j) in E, that is adjacent nodes must
be assigned different colors.
'''

Note: The MathProg model color.mod uses an heuristic to minimize the number 
      of colors to considerate, the parameter nc and z.

      I don't try to translate this heuristic, and the references to z is skipped,
      hence nc is hardcoded.

      This is, however, (still) an integer program model.


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys, math,string
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *



def coloring_ip(graph):
    n = max(graph)+1
    num_edges = len(graph)

    print("n:",n,"num_edges:",num_edges)

    # there must be no loops 
    assert [graph[i][0] != graph[i][1] for i in range(num_edges)], "No loops in the graph is allowed"


    # """
    # number of colors used by the heuristic; obviously, it is an upper
    # bound of the optimal solution 
    # """
    # hakank: However, we know that 4 colors suffice to color this planar graph...
    nc = 5


    # """
    # x[i,c] = 1 means that node i is assigned color c 
    # """
    x = boolvar(shape=(n,nc),name="x")

    # """
    # u[c] = 1 means that color c is used, i.e. assigned to some node 
    # """
    u = boolvar(shape=nc,name="u")

    # objective is to minimize the number of colors used
    obj = intvar(1,nc,name="obj")

    # constraints
    
    model = Model([obj == sum(u)])

    # each node must be assigned exactly one color 
    for i in range(n):
        model += [sum([x[i,c] for c in range(nc)]) == 1]


    # adjacent nodes cannot be assigned the same color 
    for i in range(num_edges):
        for c in range(nc):
            model += [x[graph[i][0],c] + x[graph[i][1],c] <= u[c]]

    # symmetry
    model += [u[0] == 1,
              # decreasing(u)
              ]
    

    model.minimize(obj)

    ss = CPM_ortools(model)    
    # ss.ort_solver.parameters.max_time_in_seconds = timeout
    # ss.ort_solver.parameters.num_search_workers = num_procs 
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0

    if ss.solve():
        print(ss.status())
        print("x:")
        print(1*x.value())
        print("u:",1*u.value())
        print("obj:",obj.value())
        print()


#
# This correspond to the instance myciel3.col from:
# http://mat.gsia.cmu.edu/COLOR/instances.html 
#
graph = np.array([[0,1],
                  [0,3],
                  [0,6],
                  [0,8],
                  [1,2],
                  [1,5],
                  [1,7],
                  [2,4],
                  [2,6],
                  [2,9],
                  [3,4],
                  [3,5],
                  [3,9],
                  [4,7],
                  [4,8],
                  [5,10],
                  [6,10],
                  [7,10],
                  [8,10],
                  [9,10]])

coloring_ip(graph)
