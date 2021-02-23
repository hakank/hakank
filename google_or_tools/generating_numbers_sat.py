#
# "Generating Numbers" Puzzle in OR-Tools CP-SAT
#
# https://stackoverflow.com/questions/66127644/generating-numbers-puzzle
# """
# 'Generating Numbers' Puzzle
#
# I have come across the following puzzle and couldn't formulate a solution in Picat:
#
#    You will generate 5-digit numbers, where each digit is in 1..5 and 
#    different from the others, with the constraint that any three adjacent 
#    digits used in one number can’t be used in another number. How many 
#    different numbers can be obtained according to this rule?
#
# For example, if we generated the number 23145, the next numbers cannot 
# contain 231, 314, or 145.
#
# I got really confused on how to store these "forbidden" sublists and how to 
# check each number against them as I build the list of numbers.
# """  
#
# In my StackOverflow answer, there are a lot of errors and misguided 
# approaches. Here are the two models that actually works.
# 
# A thought about this:
#   There are 60 possible triplets and each number contains
#   3 triplets: 60 / 3 = 20!
#
#   So my conjecture is that the maximum length is 20.
#   Let's search for such a sequence. 
#
#   And one should rather talk about a set of numbers since the
#   order is of no importance.
#
# Here's a length 20 set (found by my Picat models at 
# http://hakank.org/picat/generating_numbers.pi ).
# [12345,32415,34251,21435,43125,41352,24513,42153,45231,14532,
#  23541,13254,35124,31542,25314,52134,53412,15243,51423,54321]
#
# Length 20 solution (8 threads)
# ...
# walltime: 50.0945
# usertime: 50.0945
# deterministic_time: 468.187
# primal_integral: 0
# status: OPTIMAL
# [3, 4, 2, 5, 1]
# [3, 5, 1, 2, 4]
# [4, 5, 1, 3, 2]
# [5, 1, 4, 2, 3]
# [1, 3, 4, 5, 2]
# [3, 2, 4, 1, 5]
# [4, 3, 5, 2, 1]
# [2, 4, 5, 3, 1]
# [4, 1, 2, 3, 5]
# [5, 2, 3, 4, 1]
# [5, 4, 3, 1, 2]
# [1, 5, 2, 4, 3]
# [2, 1, 3, 5, 4]
# [1, 2, 5, 3, 4]
# [2, 5, 4, 1, 3]
# [3, 1, 5, 4, 2]
# [1, 4, 3, 2, 5]
# [5, 3, 2, 1, 4]
# [2, 3, 1, 4, 5]
# [4, 2, 1, 5, 3]


# WallTime: 50.094490419

#
# Model created by Hakan Kjellerstrand (hakank@gmail.com)
# See my OR-Tools models: http://hakank.org/or_tools/
#
from __future__ import print_function
import sys, math
from ortools.sat.python import cp_model as cp
from cp_sat_utils import SimpleSolutionPrinter, SimpleSolutionCounter, ListPrinter

"""
Print solutions
"""
class SolutionPrinter(cp.CpSolverSolutionCallback):
    def __init__(self, variables):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__variables = variables
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        vars = [self.Value(v) for v in self.__variables ]
        n = 5
        m = math.ceil(len(vars) / n)
        for i in range(m):
            print([vars[j] for j in range(i*5,i*5+5)])
        print()
        print()


    def SolutionCount(self):
        return self.__solution_count


def generating_numbers(m=20):

    model = cp.CpModel()
    n = 5

    x = {}
    x_flat = []
    for i in range(m):
        for j in range(n):
            x[i,j] = model.NewIntVar(1, n, f'x[{i},{j}]')
            x_flat.append(x[(i,j)])

    # Symmetry breaking (slower)
    # for i in range(n):
    #    model.Add(x[(0,i)] == i+1)

    for i in range(m):
        model.AddAllDifferent([x[i,j] for j in range(n)])
        # for j in range(i):
        for j in range(i+1,m): # faster
            for a in range(3):
                for b in range(3):
                    # model.Add(sum([ x[(i,a+k)] == x[(j,b+k)] for k in range(3)]) < 3) # This don't work!
                    # bb = [model.NewBoolVar(f'bb[{i},{j},{a},{b},{k}]') for k in range(3)]                    
                    # bb = [model.NewIntVar(0,1,f'bb[{i},{j},{a},{b},{k}]') for k in range(3)]
                    bb = [model.NewIntVar(0,1,f'bb') for k in range(3)]
                    for k in range(3):
                        #  b[k] <==> x[(i,a+k)] == x[(j,b+k)]:
                        model.Add(x[i,k+a] == x[j,k+b]).OnlyEnforceIf(bb[k])                        
                        model.Add(x[i,k+a] != x[j,k+b]).OnlyEnforceIf(bb[k].Not())
                    model.Add(sum(bb) < 3)
                    

    # print("ModelStats:", model.ModelStats())
    # print("Proto:", model.Proto())
    
    # model.AddDecisionStrategy(x_flat, 
    #                           cp.CHOOSE_FIRST,
    #                           cp.SELECT_MIN_VALUE
    #                           )

    solver = cp.CpSolver()
    # if m == 20:
    #   solver.parameters.log_search_progress = True

    solver.parameters.num_search_workers = 8
    # solution_printer = SolutionPrinter(x_flat) 
    # status = solver.SearchForAllSolutions(model, solution_printer)

    # solver.parameters.search_branching = cp.FIXED_SEARCH
    # solver.parameters.search_branching = cp.PORTFOLIO_SEARCH 
    # solver.parameters.search_branching = cp.AUTOMATIC_SEARCH 

    # solver.parameters.search_branching = cp.PORTFOLIO_SEARCH
    # solver.parameters.cp_model_presolve=False
    solver.parameters.linearization_level = 0
    # solver.parameters.cp_model_probing_level = 0

    status = solver.Solve(model) 
    print("status:", solver.StatusName(status))
    if status == cp.OPTIMAL:
        ps = []
        for i in range(m):
            xval = [solver.Value(x[i,j]) for j in range(n)] 
            print(xval)
            ps.append(int("".join([str(p) for p in xval])))
            
        print()
        print("numbers:", ps)
    print()
    print("WallTime:", solver.WallTime())
    # print('Solutions found : %i' % solution_printer.SolutionCount())
    print()
 

if __name__ == '__main__':
    if len(sys.argv) > 1:
        n = int(sys.argv[1])
        print(f"\nn:{n}")
        generating_numbers(n)
    else:
        for n in range(1,21):
            print(f"\nn:{n}")
            generating_numbers(n)
    