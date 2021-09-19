"""
TSP using circuit in cpmpy.

This model shows the solution both as the circuit 
and as the path. The assumption is that we always
start at city 0.

Example: The first instance (Nilsson) has the following 
solution (though it might be sligtly different for
a specific run of the model):

  Nilsson:
  min_val: 2 max_val: 15
  distance: 34
  x   : [2, 0, 3, 4, 5, 6, 1]
  path: [2, 3, 4, 5, 6, 1, 0]

This mean that we start at city 0 and then go to city 2 
(x[0] = 2), then to city 3 (x[2] = 3) etc. 
The path shows this path explicitly. We always end 
(i.e. come back to) city 0.


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *



def extract_path(x):
  """
  extract_path(x)

  Return the path from the circuit `x`. 

  Assumption: The start city is city 0.
  """
  n = len(x)
  start = 0 # we start at city 0 
  path = []
  for _i in range(n):
    start = x[start]
    path.append(start)
  return path


def tsp(distances, use_path=True):
  """
  tsp(distances, use_path=True)

  Solve the TSP problem using the distance matrix `distances`.

  If `use_path` = True then use `circuit_path` to also 
  extract the path.

  Note: The path can be return via the `extract_path` method.
  """

  n = len(distances)

  # variables

  # Min and max value of the distances
  min_val = min([distances[i][j] for i in range(n) for (j) in range(n) if distances[i][j] > 0])
  max_val = max([distances[i][j] for i in range(n) for j in range(n)])

  # Array version of distances (for element)
  distances_a = [0 for i in range(n) for j in range(n)]
  for i in range(n):
    for j in range(n):
      distances_a[(i*n+j)] = distances[i][j]

  x = intvar(0,n-1,shape=n,name="x") # the circuit
  if use_path:
    path = intvar(0,n-1,shape=n,name="path") # the path

  d = intvar(min_val,max_val,shape=n,name="d")
  distance = intvar(0,max_val*n,name="distance")

  model = Model(minimize=distance)

  # constraints
  model += (distance == sum([d[i] for i in range(n)]))
  if use_path:
    model += (my_circuit_path(x, path))
  else:
    model += (circuit(x))
                
  for i in range(n):
    # distances_a[i*n+x[i]] == d[i]
    ix = intvar(0,n*n)
    model += (ix == i*n+x[i])
    model += (d[i] == Element(distances_a,ix))


  ss = CPM_ortools(model)
  ss.ort_solver.parameters.num_search_workers = 12 # Don't work together with SearchForAllSolutions
  # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
  # ss.ort_solver.parameters.cp_model_presolve = False
  ss.ort_solver.parameters.linearization_level = 0
  ss.ort_solver.parameters.cp_model_probing_level = 0
  
  if ss.solve():
    print("distance:", distance.value())
    xs = x.value()
    print("x   :", xs)
    if use_path:
      print("path:", path.value())
    else:
      print("path:", extract_path(xs))
    print("status:",ss.status())

  print()


instances = {
  # From Ulf Nilsson: "Transparencies for the course TDDD08 Logic
  # Programming", page 6f
  # http://www.ida.liu.se/~TDDD08/misc/ulfni.slides/oh10.pdf
  "nilsson": [[ 0, 4, 8,10, 7,14,15],
              [ 4, 0, 7, 7,10,12, 5],
              [ 8, 7, 0, 4, 6, 8,10],
              [10, 7, 4, 0, 2, 5, 8],
              [ 7,10, 6, 2, 0, 6, 7],
              [14,12, 8, 5, 6, 0, 5],
              [15, 5,10, 8, 7, 5, 0]],

  # This instance is from the SICStus example 
  # ./library/clpfd/examples/tsp.pl
  # The "chip" examples 
  "chip":    [[0,205,677,581,461,878,345],
              [205,0,882,427,390,1105,540],
              [677,882,0,619,316,201,470],
              [581,427,619,0,412,592,570],
              [461,390,316,412,0,517,190],
              [878,1105,201,592,517,0,691],
              [345,540,470,570,190,691,0]],

# From GLPK:s example tsp.mod
# (via http://www.hakank.org/minizinc/tsp.mzn)
# """
# These data correspond to the symmetric instance ulysses16 from:
# Reinelt, G.: TSPLIB - A travelling salesman problem library.
# ORSA-Journal of the Computing 3 (1991) 376-84;
# http://elib.zib.de/pub/Packages/mp-testdata/tsp/tsplib 
# 
# The optimal solution is 6859
# """
"glpk":  [[0,509,501,312,1019,736,656,60,1039,726,2314,479,448,479,619,150],
          [509,0,126,474,1526,1226,1133,532,1449,1122,2789,958,941,978,1127,542],
          [501,126,0,541,1516,1184,1084,536,1371,1045,2728,913,904,946,1115,499],
          [312,474,541,0,1157,980,919,271,1333,1029,2553,751,704,720,783,455],
          [1019,1526,1516,1157,0,478,583,996,858,855,1504,677,651,600,401,1033],
          [736,1226,1184,980,478,0,115,740,470,379,1581,271,289,261,308,687],
          [656,1133,1084,919,583,115,0,667,455,288,1661,177,216,207,343,592],
          [60,532,536,271,996,740,667,0,1066,759,2320,493,454,479,598,206],
          [1039,1449,1371,1333,858,470,455,1066,0,328,1387,591,650,656,776,933],
          [726,1122,1045,1029,855,379,288,759,328,0,1697,333,400,427,622,610],
          [2314,2789,2728,2553,1504,1581,1661,2320,1387,1697,0,1838,1868,1841,1789,2248],
          [479,958,913,751,677,271,177,493,591,333,1838,0,68,105,336,417],
          [448,941,904,704,651,289,216,454,650,400,1868,68,0,52,287,406],
          [479,978,946,720,600,261,207,479,656,427,1841,105,52,0,237,449],
          [619,1127,1115,783,401,308,343,598,776,622,1789,336,287,237,0,636],
          [150,542,499,455,1033,687,592,206,933,610,2248,417,406,449,636,0]],
}

print("Nilsson:")
tsp(instances["nilsson"],True)
print("\nChip:")
tsp(instances["chip"],True)
print("\nGLPK, use path: False:")
tsp(instances["glpk"],False)

print("\nGLPK, use path: True:")
tsp(instances["glpk"],True)
