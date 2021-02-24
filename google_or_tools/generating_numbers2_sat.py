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
  'Generating Numbers' Puzzle in OR-Tools CP-SAT

  https://stackoverflow.com/questions/66127644/generating-numbers-puzzle
  '''
  'Generating Numbers' Puzzle

  I have come across the following puzzle and couldn't formulate a solution in Picat:

     You will generate 5-digit numbers, where each digit is in 1..5 and 
     different from the others, with the constraint that any three adjacent 
     digits used in one number can’t be used in another number. How many 
     different numbers can be obtained according to this rule?

  For example, if we generated the number 23145, the next numbers cannot 
  contain 231, 314, or 145.

  I got really confused on how to store these "forbidden" sublists and how to 
  check each number against them as I build the list of numbers.
  '''

  In my StackOverflow answer, there are a lot of errors and misguided 
  approaches.
  
  A thought about this:
    * There are 60 possible triplets and each number contains
      3 triplets: 60 / 3 = 20!

    * So my conjecture is that the maximum length is 20.
      Let's search for such a sequence. 

    * And one should rather talk about a set of numbers since the
      order is of no importance.

  This model is much faster than generating_numbers_sat.py
  It finds a solution is 0.1s (0.78s total runtime).
  Here's the first found:

        [1, 2, 3, 4, 5]
        [1, 3, 4, 2, 5]
        [1, 4, 5, 2, 3]
        [1, 5, 2, 4, 3]
        [2, 1, 5, 3, 4]
        [2, 3, 1, 5, 4]
        [2, 3, 5, 4, 1]
        [2, 5, 3, 1, 4]
        [2, 5, 4, 3, 1]
        [3, 1, 2, 4, 5]
        [3, 2, 4, 1, 5]
        [3, 4, 1, 2, 5]
        [4, 1, 3, 5, 2]
        [4, 3, 2, 5, 1]
        [4, 3, 5, 1, 2]
        [4, 5, 1, 3, 2]
        [4, 5, 3, 2, 1]
        [5, 1, 4, 2, 3]
        [5, 2, 1, 4, 3]
        [5, 4, 2, 1, 3]

        Numbers: ['12345', '13425', '14523', '15243', '21534', '23154', '23541', '25314', '25431', '31245', '32415', '34125', '41352', '43251', '43512', '45132', '45321', '51423', '52143', '54213']

        NumConflicts: 1
        NumBranches: 42
        WallTime: 0.09586971100000001


  Model created by Hakan Kjellerstrand (hakank@gmail.com)
  See my OR-Tools models: http://hakank.org/or_tools/
"""
from __future__ import print_function
import sys, math
from ortools.sat.python import cp_model as cp
from cp_sat_utils import ListPrinter
from itertools import permutations

class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""

    def __init__(self, m, perms, x):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__m = m
        self.__perms = perms
        self.__x = x        
        self.__solution_count = 0
        print("INIT DONE")

    def OnSolutionCallback(self):
        self.__solution_count += 1
        perms = self.__perms
        p_len = len(perms)
        ps = [i  for i in range(p_len) if self.Value(self.__x[i])==1]
        print("ps:", ps)
        for p in ps:
            print(self.__perms[p])
        print()
        print("Numbers:",["".join([str(pp) for pp in perms[p]]) for p in ps])
        print()


    def SolutionCount(self):
        return self.__solution_count


def triplets(p):
    """
    Generate the three triplets for permutation p
    """
    return [p[i:i+3] for i in range(3)]

def check_perm(tri1,tri2):
    """
    Check that these permutations (representing by their triplets) 
    are compatible, i.e. don't contain a common triplet.
    """
    for t1 in tri1:
        for t2 in tri2:
            if t1 == t2:
                return False
    return True
    

def generating_numbers(m=20):

    model = cp.CpModel()

    print("m:", m)

    n = 5

    # Generate the 120 permutations
    perms = [[*p] for p in (permutations(range(1,n+1)))]
    p_len = len(perms) # Well, we know it's 120.

    # Generate a matrix of compatible permutations,
    # i.e. A[P1,P2] = 1 means that Ps[P1] and Ps[P2] don't
    # have any triplets in common.
    A = {}
    for p1 in range(p_len):
        A[(p1,p1)] = 1
        tri1 = triplets(perms[p1])
        for p2 in range(p1):
            tri2 = triplets(perms[p2])
            if check_perm(tri1,tri2):
                A[(p1,p2)] = 1
                A[(p2,p1)] = 1
            else:
                A[(p1,p2)] = 0
                A[(p2,p1)] = 0

    x = [model.NewBoolVar(f"x[{i}") for i in range(p_len)]
    model.Add(sum(x) == m)
    # model.Add(x[0] == 1) # symmetry breaking
    for i in range(p_len):
        for j in range(i):
            # Should i and j be in the set?
            # (x[i] /\ x[J] ) => A[(i,j)]
            bXij = model.NewBoolVar("bXij")
            model.Add(x[i]+x[j]==2).OnlyEnforceIf(bXij)
            model.Add(x[i]+x[j]!=2).OnlyEnforceIf(bXij.Not())

            bAij = model.NewBoolVar("bAij")
            model.Add(A[(i,j)]==1).OnlyEnforceIf(bAij)
            model.Add(A[(i,j)]!=1).OnlyEnforceIf(bAij.Not())

            model.AddImplication(bXij,bAij)


    solver = cp.CpSolver()

    # solver.parameters.log_search_progress = True
    # solver.parameters.num_search_workers = 8

    # solver.parameters.search_branching = cp.PORTFOLIO_SEARCH
    # solver.parameters.cp_model_presolve=False
    # solver.parameters.linearization_level = 0
    solver.parameters.cp_model_probing_level = 0

    # solution_printer = SolutionPrinter(m, perms, x)
    # solution_printer = ListPrinter(x)
    # status = solver.SearchForAllSolutions(model, solution_printer)
    status = solver.Solve(model) 
    print("status:", solver.StatusName(status))

    if status == cp.OPTIMAL:
        ps = [i  for i in range(p_len) if solver.Value(x[i])==1]
        assert len(ps) == m, f"Length of ps ({len(ps)} is not m ({m}))"

        print("ps:", ps)
        for p in ps:
            print(perms[p])
        print()
        print("Numbers:",["".join([str(pp) for pp in perms[p]]) for p in ps])

    print()
    print("NumConflicts:", solver.NumConflicts())
    print("NumBranches:", solver.NumBranches())
    print("WallTime:", solver.WallTime())
    print()
 
m = 20
if __name__ == '__main__':
    if len(sys.argv) > 1:
        m = int(sys.argv[1])
    generating_numbers(m)
