#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# TSP using circuit in Z3
#

# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *



def tsp_circuit(distances):

  n = len(distances)
  print("n:",n)

  sol = SimpleSolver()

  # variables
  min_val = min([distances[i][j] for i in range(n) for (j) in range(n) if distances[i][j] > 0])
  max_val = max([distances[i][j] for i in range(n) for j in range(n)])

  print("min_val:",min_val,"max_val:",max_val)

  distances_flatten = [distances[i][j] for i in range(n) for j in range(n)]

  x = makeIntVector(sol,"x",n, 0,n-1) # the circuit
  path = makeIntVector(sol,"path",n, 0,n-1) # the circuit

  d = makeIntVector(sol,"d",n,min_val,max_val)
  distance = Int("distance")

  # constraints
  sol.add(distance == Sum([d[i] for i in range(n)]))

  # sol.add(Distinct([x[i] for i in range(n)])) # done in circuit
  circuit2(sol,x, path, n) 

  for i in range(n):
    # sol.add(d[i] == distances_a[i*n + x[i]])
    element(sol,i*n + x[i], distances_flatten, d[i],n*n)

  print("solve")
  # print(sol)
  num_solutions = 0
  print(sol.check())
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print("distance: ", mod.eval(distance))
    print("x   :", [mod.eval(x[i]) for i in range(n)])
    print("path:", [mod.eval(path[i]) for i in range(n)])
    print("d   :", [mod.eval(d[i]) for i in range(n)])
    print()
    getDifferentSolution(sol,mod,[x[i] for i in range(n)])
    getDifferentSolution(sol,mod,[d[i] for i in range(n)])
    getDifferentSolution(sol,mod,[path[i] for i in range(n)])        
    getLessSolution(sol,mod, distance)


# 
# data
#


# This problem is from 
# GLPK:s example tsp.mod
# (via http://www.hakank.org/minizinc/tsp.mzn)
# """
# These data correspond to the symmetric instance ulysses16 from:
# Reinelt, G.: TSPLIB - A travelling salesman problem library.
# ORSA-Journal of the Computing 3 (1991) 376-84;
# http://elib.zib.de/pub/Packages/mp-testdata/tsp/tsplib 
# 
# The optimal solution is 6859
# """
#
glpk = [
    [   0,  509,  501,  312, 1019,  736,  656,   60, 1039,  726, 2314,  479,  448,  479,  619,  150],
    [ 509,    0,  126,  474, 1526, 1226, 1133,  532, 1449, 1122, 2789,  958,  941,  978, 1127,  542],
    [ 501,  126,    0,  541, 1516, 1184, 1084,  536, 1371, 1045, 2728,  913,  904,  946, 1115,  499],
    [ 312,  474,  541,    0, 1157,  980,  919,  271, 1333, 1029, 2553,  751,  704,  720,  783,  455],
    [1019, 1526, 1516, 1157,    0,  478,  583,  996,  858,  855, 1504,  677,  651,  600,  401, 1033],
    [ 736, 1226, 1184,  980,  478,    0,  115,  740,  470,  379, 1581,  271,  289,  261,  308,  687],
    [ 656, 1133, 1084,  919,  583,  115,    0,  667,  455,  288, 1661,  177,  216,  207,  343,  592],
    [  60,  532,  536,  271,  996,  740,  667,    0, 1066,  759, 2320,  493,  454,  479,  598,  206],
    [1039, 1449, 1371, 1333,  858,  470,  455, 1066,    0,  328, 1387,  591,  650,  656,  776,  933],
    [ 726, 1122, 1045, 1029,  855,  379,  288,  759,  328,    0, 1697,  333,  400,  427,  622,  610],
    [2314, 2789, 2728, 2553, 1504, 1581, 1661, 2320, 1387, 1697,    0, 1838, 1868, 1841, 1789, 2248],
    [ 479,  958,  913,  751,  677,  271,  177,  493,  591,  333, 1838,    0,   68,  105,  336,  417],
    [ 448,  941,  904,  704,  651,  289,  216,  454,  650,  400, 1868,   68,    0,   52,  287,  406],
    [ 479,  978,  946,  720,  600,  261,  207,  479,  656,  427, 1841,  105,   52,    0,  237,  449],
    [ 619, 1127, 1115,  783,  401,  308,  343,  598,  776,  622, 1789,  336,  287,  237,    0,  636],
    [ 150,  542,  499,  455, 1033,  687,  592,  206,  933,  610, 2248,  417,  406,  449,  636,    0]
    ]

#
# From Ulf Nilsson:
# Transparencies for the course TDDD08 Logic Programming"
#
nilsson = [[ 0, 4, 8,10, 7,14,15],
           [ 4, 0, 7, 7,10,12, 5],
           [ 8, 7, 0, 4, 6, 8,10],
           [10, 7, 4, 0, 2, 5, 8],
           [ 7,10, 6, 2, 0, 6, 7],
           [14,12, 8, 5, 6, 0, 5],
           [15, 5,10, 8, 7, 5, 0]]

#
# This problem is from the SICStus example 
# ./library/clpfd/examples/tsp.pl
# The "ilog" examples
#
ilog = [[2,4,4,1,9,2,4,4,1,9],
        [2,9,5,5,5,2,9,5,5,5],
        [1,5,2,3,3,1,5,2,3,3],
        [2,6,8,9,5,2,6,8,9,5],
        [3,7,1,6,4,3,7,1,6,4],
        [1,2,4,1,7,1,2,4,1,7],
        [3,5,2,7,6,3,5,2,7,6],
        [2,7,9,5,5,2,7,9,5,5],
        [3,9,7,3,4,3,9,7,3,4],
        [4,1,5,9,2,4,1,5,9,2]]

#
# This problem is from the SICStus example 
# ./library/clpfd/examples/tsp.pl
# The "chip" examples
#
chip = [[  0, 205, 677, 581, 461, 878, 345],
        [205,   0, 882, 427, 390,1105, 540],
        [677, 882,   0, 619, 316, 201, 470],
        [581, 427, 619,   0, 412, 592, 570],
        [461, 390, 316, 412,   0, 517, 190],
        [878,1105, 201, 592, 517,   0, 691],
        [345, 540, 470, 570, 190, 691,   0]]



print("chip:")
tsp_circuit(chip)

print("\nnilsson:")
tsp_circuit(nilsson)

print("\nilog:")
tsp_circuit(ilog)

# This is quite slow
# print("\nglpk:")
# tsp_circuit(glpk)
