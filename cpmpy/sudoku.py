"""
Sudoku problem in cpmpy.

This is a straightforward implementation of Sudoku.

For more about Sudoku see:
http://en.wikipedia.org/wiki/Sudoku

See sudoku_problems.py for the instances which was taken from my Picat model
http://hakank.org/picat/sudoku.pi . Mostly it is from the Gecode's collection
of Sudoku instances of size 9x9, 16x16, and 25x25: 
http://www.gecode.org/gecode-doc-latest/sudoku08cpp-source.html

All 9x9 and 16x16 are solved immediately with the OR-tools CP-SAT solver,
so the interesting instances are the 25x25 instances. 

Below are times both for first solution and the of proving unicity of 
the solution, i.e. check for 2 solutions. This is slower by a factor
of about 2. We experiment with some parameter settings:
    
    solver.parameters.linearization_level = 0
and
    solver.parameters.cp_model_probing_level = 0


Only first solution (not proving unicity)
-----------------------------------------
  * Total time: 58.957s

Proving Unicity (trying to get 2 solutions):
----------------
  * Total time: 2min 58.22s

Compare with the 'plain' OR-Tools CP-SAT model
https://github.com/hakank/hakank/blob/master/google_or_tools/sudoku_sat.py
which - unsurprisingly - give about the same times.


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/

"""
import sys,math
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *
from sudoku_problems import all_sudoku_problems


def sudoku(puzzle,num_sols=1,num_procs=1):
    n = len(puzzle[0])
    
    x = intvar(1,n,shape=(n,n), name="x")
    x_flat = [x[i,j] for i in range(n) for j in range(n)]
    
    model = Model()

    #
    # set the clues
    #
    for i in range(0,n):
        for j in range(0,n):
            if puzzle[i][j] > 0:
                model += [x[i,j] == puzzle[i][j]]

    #
    # rows and columns must be different
    #
    model += [[AllDifferent(row) for row in x],
              [AllDifferent(col) for col in x.transpose()]
              ]

    #
    # the cells (regions) must be different
    #
    reg = math.ceil(math.sqrt(n)) # size of region
    for i in range(reg):
        for j in range(reg):
            model += [AllDifferent([x[r,c] for r in range(i*reg,i*reg+reg) for c in range(j*reg,j*reg+reg)])]

    def print_sol():
        for i in range(n):
            for j in range(n):
                print(f"{x[i][j].value():3}", end=" ")
            print()
        print()

    s = CPM_ortools(model)
    # Note that we have to use a flattened version of x.
    cb = ORT_simple_printer_matrix(s._varmap,x_flat,n,n,num_sols)

    # if num_procs > 1:
    #     print("number of processes:", num_procs)
    #     s.ort_solver.parameters.num_search_workers = num_procs
    s.ort_solver.parameters.num_search_workers = num_procs

    # Flags to experiment with        
    # s.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # s.ort_solver.parameters.cp_model_presolve = False
    if num_sols > 1:    
        s.ort_solver.parameters.linearization_level = 0
    if num_sols == 1:    
        s.ort_solver.parameters.cp_model_probing_level = 0

    if num_sols > 1:
        ort_status = s.ort_solver.SearchForAllSolutions(s.ort_model, cb)
    else:
        ort_status = s.ort_solver.Solve(s.ort_model, cb)

    # print("s.status():", s.status())
    print("Nr solutions:", cb.solcount)
    print("Num conflicts:", s.ort_solver.NumConflicts())
    print("NumBranches:", s.ort_solver.NumBranches())
    print("WallTime:", s.ort_solver.WallTime())
    
    return s.ort_solver.WallTime()


def run_sudoku_problems(size=9,num_sols=2,num_workers=1):
  """
  run_sudoku_problems(size=9)

  Run all Sudoku problems of size `size`.

  Returns a list of the wall times.
  """
  times = []
  for p in sorted(all_sudoku_problems.keys()):
    prob = all_sudoku_problems[p]
    if len(prob[0])== size:
      print(f"\nRun #{p}:")
      t = sudoku(prob,num_sols,num_workers)
      print(f"Problem #{p}:", t)
      times.append((p,size,t))
  return times


num_sols = 1 # 2: Prove unicity  1: Just the first solution
num_workers = 1 # If > 1 it only give the first solution.

all_times = []
print("\n\nSize 9:")
times9 = run_sudoku_problems(9,num_sols,num_workers)
for pt in times9:
    all_times.append(pt)

print("\n\nSize 16:")
times16 = run_sudoku_problems(16,num_sols,num_workers)
for pt in times16:
    all_times.append(pt)

print("\n\nSize 25:")
times25 = run_sudoku_problems(25,num_sols,num_workers)
for pt in times25:
    all_times.append(pt)

for pt in all_times:
    print(pt)

