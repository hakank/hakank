# Copyright 2021 Hakan Kjellerstrand hakank@gmail.com
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
"""
	
	TSP using circuit in OR-tools CP-SAT Solver.

	This model shows the solution both as the circuit 
	and as the path. The assumption is that we always
	start at city 0.

	In contrast to tsp_circuit_sat.py, it use the built-in
	constraint AddCircuit which is much faster. (It is 
	also heavily inspired by the example 
	examples/python/tsp_sat.py from the or-tools distribution).

	Example: The first instance (Nilsson) has the following 
	solution (though it might be sligtly different for
	a specific run of the model):

		Nilsson:
		distance: 34.0
		Route: 0 -> 2 -> 3 -> 4 -> 5 -> 6 -> 1 -> 0
		Travelled distance: 34

	I.e.: we start at city 0 and then go to city 2 
	(x[0] = 2), then to city 3 (x[2] = 3) etc. 
	We always end (i.e. come back to) city 0.
	

	This model was created by Hakan Kjellerstrand, hakank@gmail.com
	See also my OR-tools page: http://www.hakank.org/or_tools/

"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *


def tsp(distances):
	"""
	tsp(distances)

	Solve the TSP problem using the distance matrix `distances`.

	If `use_path` = True then use `circuit_path` to also 
	extract the path.

	Note: The path can be return via the `extract_path` method.
	"""

	model = cp.CpModel()

	n = len(distances)

	# variables

	# constraints

	# Create the circuit constraint.         
	obj_vars = []
	obj_coeffs = []
																												
	arcs = []
	arc_literals = {}
	for i in range(n):
	  for j in range(n):
	    if i == j:
	      continue

	    lit = model.NewBoolVar('%i follows %i' % (j, i))
	    arcs.append([i, j, lit])
	    arc_literals[i, j] = lit

	    obj_vars.append(lit)
	    obj_coeffs.append(distances[i][j])

	model.AddCircuit(arcs)

	# Minimize weighted sum of arcs. Because this s                                                         
	model.Minimize(
			sum(obj_vars[i] * obj_coeffs[i] for i in range(len(obj_vars))))

	solver  = cp.CpSolver()
	solver.parameters.num_search_workers = 8
	# solver.parameters.search_branching = cp.PORTFOLIO_SEARCH
	# solver.parameters.cp_model_presolve = False
	solver.parameters.linearization_level = 0
	solver.parameters.cp_model_probing_level = 0

	status = solver.Solve(model)
	if status == cp.OPTIMAL:
		print("distance:", solver.ObjectiveValue())
		current_node = 0
		str_route = '%i' % current_node
		route_is_finished = False
		route_distance = 0
		while not route_is_finished:
			for i in range(n):
				if i == current_node:
					continue
				if solver.BooleanValue(arc_literals[current_node, i]):
					str_route += ' -> %i' % i
					route_distance += distances[current_node][i]
					current_node = i
					if current_node == 0:
							route_is_finished = True
					break

		print('Route:', str_route)
		print('Travelled distance:', route_distance)

	print()
	print("NumConflicts:", solver.NumConflicts())
	print("NumBranches:", solver.NumBranches())
	print("WallTime:", solver.WallTime())


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

if __name__ == '__main__':
	print("Nilsson:")
	tsp(instances["nilsson"])
	print("\nChip:")
	tsp(instances["chip"])
	print("\nGLPK:")
	tsp(instances["glpk"])
