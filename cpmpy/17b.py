"""
17x17 challenge in cpmpy.

From Karsten Konrad:
http://lookforlight.tumblr.com/post/996786415/lets-do-real-cp-forbiddenassignment
'''
/*********************************************
 * The n x m grid is c-colorable if there is a way
 * to c-color the vertices of the n x m grid so that
 * there is no rectangle with all four corners the
 * same color. (The rectangles I care about have the
 * sides parallel to the x and y axis.)
 *
 *
 * Is there a 17x17 solution?
 * see: http://blog.computationalcomplexity.org/2009/11/17x17-challenge-worth-28900-this-is-not.html
 *
 * OPL 6.3 Model
 * Author: karsten.konrad
 * Creation Date: 19.08.2010 at 10:39:14
 *********************************************/
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

def model_17_b(nb_rows=10,nb_columns=10,nb_colors=4,num_sols=0,num_procs=1):
    
    print(nb_rows,"x", nb_columns, "x", nb_colors, "num_sols:", num_sols, "num_procs:",num_procs)
    
    space = intvar(0,nb_colors-1,shape=(nb_rows,nb_columns),name="space")

    model = Model (
        # symmetry breaking
        # space[0,0] == 0,
        # space[nb_rows-1,nb_columns-1] == 1,

        [atmost([space[r,c],space[r2,c],space[r,c2],space[r2,c2]], val, nb_colors-1)
                      for r in range(nb_rows)
                      for r2 in range(r)
                      for c in range(nb_columns)
                      for c2 in range(c)
                      for val in range(nb_colors)]
        )

    print("Search")
    
    def print_sol():
        print(space.value())
        print()

    ss = CPM_ortools(model)

    if num_sols == 1 and num_procs > 1:
        print(f"Num workers: {num_procs}")
        ss.ort_solver.parameters.num_search_workers = num_procs

    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH 
    # ss.ort_solver.parameters.cp_model_presolve = False
    ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0

    num_solutions = ss.solveAll(solution_limit=num_sols,display=print_sol)
    print("num_solutions:", num_solutions)
    print("Num conflicts:", ss.ort_solver.NumConflicts())
    print("NumBranches:", ss.ort_solver.NumBranches())
    print("WallTime:", ss.ort_solver.WallTime())

nb_rows    = 13
nb_columns = 13
nb_colors  = 4
num_sols   = 1
num_procs  = 1 # 12 
model_17_b(nb_rows,nb_columns,nb_colors,num_sols,num_procs)


