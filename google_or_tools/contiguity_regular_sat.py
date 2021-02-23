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

  Global constraint contiguity using regular in OR-tools CP-SAT Solver.

  This is a decomposition of the global constraint
  global contiguity.

  From Global Constraint Catalogue
  http://www.emn.fr/x-info/sdemasse/gccat/Cglobal_contiguity.html
  '''
  Enforce all variables of the VARIABLES collection to be assigned to 0 or 1.
  In addition, all variables assigned to value 1 appear contiguously.

  Example:
  (<0, 1, 1, 0>)

  The global_contiguity constraint holds since the sequence 0 1 1 0 contains
  no more than one group of contiguous 1.
  '''

  This is a port of my old CP model contiguity_regular.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/

"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import regular_element, regular_table



class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, reg_input):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__reg_input = reg_input
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        # Note: We subtract 1 from the value
        print([self.Value(self.__reg_input[i])-1 for i in range(len(self.__reg_input))])

    def SolutionCount(self):
        return self.__solution_count


def main(n=7):

  model = cp.CpModel()

  #
  # data
  #
  # the DFA (for regular)
  n_states = 3
  input_max = 2
  initial_state = 1  # 0 is for the failing state

  # all states are accepting states
  accepting_states = [1, 2, 3]

  # The regular expression 0*1*0*
  transition_fn = [
      [1, 2],  # state 1 (start): input 0 -> state 1, input 1 -> state 2 i.e. 0*
      [3, 2],  # state 2: 1*
      [3, 0],  # state 3: 0*
  ]

  #
  # declare variables
  #

  # We use 1..2 and subtract 1 in the solution
  reg_input = [model.NewIntVar(1, 2, 'x[%i]' % i) for i in range(n)]

  #
  # constraints
  #

  # regular_element(model, reg_input, n_states, input_max, transition_fn, initial_state,
  #        accepting_states)
  # Much faster:
  regular_table(model, reg_input, n_states, input_max, transition_fn, initial_state,
          accepting_states)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  solution_printer = SolutionPrinter(reg_input)
  status = solver.SearchForAllSolutions(model, solution_printer)
  
  if not (status == cp.OPTIMAL or status == cp.FEASIBLE):
    print("No solution!")

  print()
  print('NumSolutions:', solution_printer.SolutionCount())
  print('NumConflicts:', solver.NumConflicts())
  print('NumBranches:', solver.NumBranches())
  print('WallTime:', solver.WallTime())


n = 7
if __name__ == "__main__":
  if len(sys.argv) > 1:
    n = int(sys.argv[1])

  main(n)
