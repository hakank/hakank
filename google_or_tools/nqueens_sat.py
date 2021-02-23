#!/usr/bin/env python3 -u
#
# N-queens problem in OR-tools CP-SAT
# 
# The All different approach was inspired 
# by https://developers.google.com/optimization/cp/queens
#
#
#
# Model created by Hakan Kjellerstrand (hakank@gmail.com)
# See my OR-Tools models: http://hakank.org/or_tools/
# 
from __future__ import print_function
import sys
from ortools.sat.python import cp_model as cp
from cp_sat_utils import SimpleSolutionPrinter, SimpleSolutionCounter, ListPrinter

def main(n, use_all_diff=1,print_sols=1):
    model = cp.CpModel()
    queens = [model.NewIntVar(0, n - 1, f'x{i}')
              for i in range(n)]
    
    model.AddAllDifferent(queens)
    if use_all_diff == 1:
        print("Use All Different")
        for i in range(n):
            diag1 = []
            diag2 = []
            for j in range(n):
                q1 = model.NewIntVar(0, 2 * n, f'diag1_{i}')
                diag1.append(q1)
                model.Add(q1 == queens[j] + j)
                q2 = model.NewIntVar(-n, n, f'diag2_{i}')
                diag2.append(q2)
                model.Add(q2 == queens[j] - j)
            model.AddAllDifferent(diag1)
            model.AddAllDifferent(diag2)
    else:
        # Alternative (naive) model (see nqueens.py)
        print("Use Naive")
        for i in range(n):
            for j in range(i):
                model.Add(queens[i] != queens[j])
                model.Add(queens[i] + i != queens[j] + j)
                model.Add(queens[i] - i != queens[j] - j)

    # print("model:", model) # Show the model
    
    solver = cp.CpSolver()
    
    # solution_printer = DiagramPrinter(queens) # Nicer output
    solution_printer = ListPrinter(queens) # List of integers
    if print_sols != 1:
        solution_printer = SimpleSolutionCounter(queens) # Just count solutions
    _ = solver.SearchForAllSolutions(model, solution_printer)
    print()
    print('Solutions found : %i' % solution_printer.SolutionCount())
    print()
    # print(solver.ResponseStats())
    print("WallTime:", solver.WallTime())


def benchmark(n_from, n_to, use_all_diff=1):
    for n in range(n_from,n_to+1):
        print(f"\nn:{n}")
        main(n, use_all_diff)
        

#
# Nicer output
#
class DiagramPrinter(cp.CpSolverSolutionCallback):
    def __init__(self, variables):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__variables = variables
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1

        for v in self.__variables:
            queen_col = int(self.Value(v))
            n = len(self.__variables)
            # Print row with queen.
            for j in range(n):
                if j == queen_col:
                    # There is a queen in column j, row i.
                    print("Q", end=" ")
                else:
                    print("_", end=" ")
            print()
        print()

    def SolutionCount(self):
        return self.__solution_count



if __name__ == '__main__':
    n = 8
    use_all_diff=1
    print_sols = 1
    if len(sys.argv) > 1:
        n = int(sys.argv[1])
    if len(sys.argv) > 2:
        use_all_diff = int(sys.argv[2])
    if len(sys.argv) > 3:
        print_sols = int(sys.argv[3])

    print(f"n:{n} use_all_diff:{use_all_diff} print_sols:{print_sols}")
    main(n,use_all_diff)

    # print("AllDifferent")
    # benchmark(10,15,1)
    # print("\nNaive:")
    # benchmark(10,15,0)    
