"""
N-queens problem in CPMpy

CSPlib prob054

Problem description from the numberjack example:
The N-Queens problem is the problem of placing N queens on an N x N chess
board such that no two queens are attacking each other. A queen is attacking
another if it they are on the same row, same column, or same diagonal.

Here are some different approaches with different version of both
the constraints and how to solve and print all solutions.


This CPMpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my CPMpy page: http://hakank.org/cpmpy/

"""
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


#
# This is a simple version without any fancy OR-tools stuff.
#
def nqueens_v1(n=8,num_sols=0):
    print(f"nqueens_v2(n={n},num_sols={num_sols})")
    queens = IntVar(1,n, shape=n)

    # Constraints on columns and left/right diagonal
    model = Model([
        AllDifferent(queens),
        AllDifferent([queens[i] - i for i in range(n)]),
        AllDifferent([queens[i] + i for i in range(n)]),
    ])

    def print_sol():
       print([queens[i].value() for i in range(n)])

    num_solutions = model.solveAll(solution_limit=num_sols,display=print_sol)
    print("num_solutions:", num_solutions)

#
# Using OR-tools flags and the new style.
# This is faster than nqueens_v1.
#
def nqueens_v2(n=8,num_sols=0):
    print(f"nqueens_v2(n={n},num_sols={num_sols})")
    queens = IntVar(1,n, shape=n)

    # Constraints on columns and left/right diagonal
    model = Model([
        AllDifferent(queens),
        AllDifferent([queens[i] - i for i in range(n)]),
        AllDifferent([queens[i] + i for i in range(n)]),
    ])

    # all solution solving, with blocking clauses
    ss = CPM_ortools(model)
    
    # ss.ort_solver.parameters.num_search_workers = 8 # not for SearchForAllSolutions!

    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH 
    # ss.ort_solver.parameters.cp_model_presolve = False
    # ss.ort_solver.parameters.linearization_level = 0
    # ss.ort_solver.parameters.cp_model_probing_level = 0

    num_solutions = 0
    while(ss.solve()):
        print([queens[i].value() for i in range(n)])        
        num_solutions += 1
        # add blocking clause and solution hint
        if num_sols > 0 and num_solutions >= num_sols:
            break
        else:
            ss += [ any(queens != queens.value()) ]
            # ss.solution_hint(queens, queens.value())

    print("num_solutions:", num_solutions)
    print("Num conflicts:", ss.ort_solver.NumConflicts())
    print("NumBranches:", ss.ort_solver.NumBranches())
    print("WallTime:", ss.ort_solver.WallTime())


def nqueens_v3(n=8,num_sols=0):
    print(f"nqueens_v3(n={n},num_sols={num_sols})")
    queens = IntVar(1,n, shape=n)

    # Constraints on columns and left/right diagonal
    model = Model([
        AllDifferent(queens),
        AllDifferent([queens[i] - i for i in range(n)]),
        AllDifferent([queens[i] + i for i in range(n)]),
    ])

    # all solution solving, with blocking clauses
    ss = CPM_ortools(model)    

    cb = ORT_simple_printer(ss._varmap,queens,num_sols)

    # Flags to experiment with
    # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0

    ort_status = ss.ort_solver.SearchForAllSolutions(ss.ort_model, cb)
    # print(ss._after_solve(ort_status)) # post-process after solve() call...
    print(ss.status())
    print("Nr solutions:", cb.solcount)
    print("Num conflicts:", ss.ort_solver.NumConflicts())
    print("NumBranches:", ss.ort_solver.NumBranches())
    print("WallTime:", ss.ort_solver.WallTime())
 

#
# Naive version, i.e. not using all_different/1.
#
def nqueens_naive(n=8,num_sols=0):
    print(f"nqueens_naive(n={n},num_sols={num_sols})")
    queens = IntVar(1,n, shape=n)

    model = Model()
    for i in range(n):
        for j in range(i):
            model += [queens[i] != queens[j],
                  queens[i] + i != queens[j] + j,
                  queens[i] - i != queens[j] - j,
                  ]            

    ss = CPM_ortools(model)    
    cb = ORT_simple_printer(ss._varmap,queens,num_sols)

    # Flags to experiment with
    # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0

    ort_status = ss.ort_solver.SearchForAllSolutions(ss.ort_model, cb)
    # print(ss._after_solve(ort_status)) # post-process after solve() call...
    print(ss.status())
    print("Nr solutions:", cb.solcount)
    print("Num conflicts:", ss.ort_solver.NumConflicts())
    print("NumBranches:", ss.ort_solver.NumBranches())
    print("WallTime:", ss.ort_solver.WallTime())
 


nqueens_v1(8,0) # 4.854s
print()
nqueens_v2(8,0) # 1.990s with hints: 1.843s
print()
nqueens_v3(8,0) # 0.251s!
print()
nqueens_naive(8,0) # 0.228s
print()

# Test larger instances, time to first solution
# nqueens(100,1) # 11.481s
print()
nqueens_v2(100,1) # 10.962s With linearization_level = 0 and cp_model_probing_level = 0: 0.956s
print()
nqueens_v3(100,1) # 11.574s  With linearization_level = 0 and cp_model_probing_level = 0: 0.977s
# nqueens_v3(1000,1) # > 4mins
print()
nqueens_naive(100,1) # With linearization_level = 0 and cp_model_probing_level = 0: 6,291

# First 2 solutions:
print()

# With linearization_level = 0 and cp_model_probing_level = 0: 1.29842271s
# num_solutions: 2
# Num conflicts: 211
# NumBranches: 9966
# WallTime: 1.29842271
nqueens_v2(100,2) 
print()

# With linearization_level = 0 and cp_model_probing_level = 0: 1.0542986s
# Nr solutions: 2
# Num conflicts: 118
# NumBranches: 7553
# WallTime: 1.0542986
nqueens_v3(100,2) 
print()

# With linearization_level = 0 and cp_model_probing_level = 0: 4.805541327s
# Nr solutions: 2
# Num conflicts: 2126
# NumBranches: 24128
# WallTime: 4.805541327
nqueens_naive(100,2) 
