# Copyright 2021 Hakan Kjellerstrand hakank@bonetmail.com
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
    All different except 0 Google CP-SAT Solver.

    Decomposition of global constraint alldifferent_except_0.

    From Global constraint catalogue:
    http://www.emn.fr/x-info/sdemasse/gccat/Calldifferent_except_0.html
    '''
    Enforce all variables of the collection VARIABLES to take distinct
    values, except those variables that are assigned to 0.

    Example
        (<5, 0, 1, 9, 0, 3>)

    The alldifferent_except_0 constraint holds since all the values
    (that are different from 0) 5, 1, 9 and 3 are distinct.
    '''

    This is a port of my old OR-tools CP model alldifferent_except_0.py

    Hmm, for n=7:
    - old CP model (alldifferent_except_0.py): 0.202s
    - this CP-SAT model: 0.46s 
      See the parameters below that speed up things considerably.
    

    This model was created by Hakan Kjellerstrand (hakank@gmail.com)
    Also see my other Google OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import alldifferent_except_0, count_vars, ListPrinter

def main(n=7):
    model = cp.CpModel()

    x = [model.NewIntVar(0, n-1, 'x%i' % i) for i in range(n)]

    alldifferent_except_0(model, x)

    # Require that there should be exactly 2 0s.
    # z = model.NewIntVar(0,n-1,'z')
    # count_vars(model,x, 0, z)
    # model.Add(z == 2)
    # Simpler:
    count_vars(model,x, 0, 2)


    # model.AddDecisionStrategy(x, 
    #                          cp.CHOOSE_MIN_DOMAIN_SIZE,
    #                          # cp.CHOOSE_FIRST,
    #                          cp.SELECT_LOWER_HALF
    #                          # cp.SELECT_MIN_VALUE
    #                        )
    
 
    solver = cp.CpSolver()
    # solver.parameters.search_branching = cp.FIXED_SEARCH
    # solver.parameters.search_branching = cp.AUTOMATIC_SEARCH

    # These speeds things up:
    solver.parameters.search_branching = cp.PORTFOLIO_SEARCH
    solver.parameters.cp_model_presolve=False
    # solver.parameters.linearization_level = 0
    # solver.parameters.cp_model_probing_level = 0

    # flat = x 
    # flat.append(z)
    solution_printer = ListPrinter(x)
    # solution_printer = SimpleSolutionCounter(x)
    status = solver.SearchForAllSolutions(model, solution_printer)
    print('Solutions found : %i' % solution_printer.SolutionCount())
    # status = solver.Solve(model)
    # if status == cp.FEASIBLE or status == cp.OPTIMAL:
    #     print([int(solver.Value(x[i])) for i in range(n)])

    print('Statistics:')
    print('  - conflicts : %i' % solver.NumConflicts())
    print('  - branches  : %i' % solver.NumBranches())
    print('  - wall time : %f s' % solver.WallTime())


if __name__ == "__main__":
    n = 7 
    if len(sys.argv) > 1:
        n = int(sys.argv[1])
    print(f"\nn:{n}")
    main(n)
