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

  Nonogram (Painting by numbers) in OR-tools CP-SAT Solver.

  http://en.wikipedia.org/wiki/Nonogram
  '''
  Nonograms or Paint by Numbers are picture logic puzzles in which cells in a
  grid have to be colored or left blank according to numbers given at the
  side of the grid to reveal a hidden picture. In this puzzle type, the
  numbers measure how many unbroken lines of filled-in squares there are
  in any given row or column. For example, a clue of '4 8 3' would mean
  there are sets of four, eight, and three filled squares, in that order,
  with at least one blank square between successive groups.

  '''

  See problem 12 at http://www.csplib.org/.

  http://www.puzzlemuseum.com/nonogram.htm

  Haskell solution:
  http://twan.home.fmf.nl/blog/haskell/Nonograms.details

  Brunetti, Sara & Daurat, Alain (2003)
  'An algorithm reconstructing convex lattice sets'
  http://geodisi.u-strasbg.fr/~daurat/papiers/tomoqconv.pdf


  The Comet model (http://www.hakank.org/comet/nonogram_regular.co)
  was a major influence when writing this model.

  I have also blogged about the development of a Nonogram solver in Comet
  using the regular constraint.
  * 'Comet: Nonogram improved: solving problem P200 from 1:30 minutes
     to about 1 second'
     http://www.hakank.org/constraint_programming_blog/2009/03/comet_nonogram_improved_solvin_1.html

  * 'Comet: regular constraint, a much faster Nonogram with the regular constraint,
     some OPL models, and more'
     http://www.hakank.org/constraint_programming_blog/2009/02/comet_regular_constraint_a_muc_1.html


  This is a the same as my CP-SAT model nonogram_table_sat.py
  but it use AddAutomaton instead if regular_table. 
  And what I can see it's much faster.

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/

"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import SimpleSolutionPrinter

class SolutionPrinter(cp.CpSolverSolutionCallback):
    """
    SolutionPrinter for Nonogram.
    """
    def __init__(self, board, rows, cols,):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__board = board
        self.__rows = rows
        self.__cols = cols
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        print()
        for i in range(self.__rows):
          row = [self.Value(self.__board[i, j])  for j in range(self.__cols)]
          row_pres = []
          for j in row:
            if j == 1:
              row_pres.append('#')
            else:
              row_pres.append(' ')
          print('  ', ''.join(row_pres))
        print()

        if self.__solution_count >= 2:
          print('2 solutions is enough...')
          self.StopSearch()

    def SolutionCount(self):
        return self.__solution_count


def make_transition_automaton(pattern):
  """
  Make a transition (automaton) matrix from a
  single pattern, e.g. [3,2,1]
  """
  t = []
  c = 0
  for i in range(len(pattern)):
    t.append((c,0,c))  # 0*
    for _j in range(pattern[i]):      
      t.append((c,1,c+1)) # 1{pattern[i]}
      c += 1

    t.append((c,0,c+1)) # 0+
    c+=1
  
  t.append((c,0,c)) # 0*
  
  return t, c



def check_rule(model, rules, y):
  """
  Check each rule by creating an automaton
  and then run the regular constraint.
  """

  rules_tmp = []
  for i in range(len(rules)):
    if rules[i] > 0:
      rules_tmp.append(rules[i])

  transitions, last_state = make_transition_automaton(rules_tmp)

  initial_state = 0
  accepting_states = [last_state-1,last_state]

  #
  # constraints
  #
  model.AddAutomaton(y, initial_state, accepting_states, transitions)


def main(rows, row_rule_len, row_rules, cols, col_rule_len, col_rules):
  """
  Run a Nonogram instance.
  """
  model = cp.CpModel()

  #
  # data
  #

  #
  # variables
  #
  board = {}
  for i in range(rows):
    for j in range(cols):
      board[i, j] = model.NewBoolVar('board[%i, %i]' % (i, j))

  # Flattened board for labeling (for testing fixed strategy).
  # This labeling was inspired by a suggestion from
  # Pascal Van Hentenryck about my Comet nonogram model.
  board_label = []
  if rows * row_rule_len < cols * col_rule_len:
    for i in range(rows):
      for j in range(cols):
        board_label.append(board[i, j])
  else:
    for j in range(cols):
      for i in range(rows):
        board_label.append(board[i, j])

  #
  # constraints
  #
  for i in range(rows):
    check_rule(model, [row_rules[i][j] for j in range(row_rule_len)],
               [board[i, j] for j in range(cols)])

  for j in range(cols):
    check_rule(model, [col_rules[j][k] for k in range(col_rule_len)],
               [board[i, j] for i in range(rows)])

  # model.AddDecisionStrategy(board_label, 
  #                           cp.CHOOSE_LOWEST_MIN, # cp.CHOOSE_FIRST,
  #                           cp.SELECT_LOWER_HALF # cp.SELECT_MIN_VALUE
  #                           )


  #
  # solution and search
  #
  solver = cp.CpSolver()
  # solver.parameters.search_branching = cp.PORTFOLIO_SEARCH
  # solver.parameters.search_branching = cp.FIXED_SEARCH
  # solver.parameters.cp_model_presolve = False
  solver.parameters.linearization_level = 0
  solver.parameters.cp_model_probing_level = 0

  solution_printer = SolutionPrinter(board, rows, cols)
  status = solver.SearchForAllSolutions(model, solution_printer)

  print("status:", solver.StatusName(status))
  if not (status == cp.OPTIMAL or status == cp.FEASIBLE):
    print("No solution")

  print()
  print('NumSolutions:', solution_printer.SolutionCount())
  print('NumConflicts:', solver.NumConflicts())
  print('NumBranches:', solver.NumBranches())
  print('WallTime:', solver.WallTime())


#
# Default problem
#
# From http://twan.home.fmf.nl/blog/haskell/Nonograms.details
# The lambda picture
#
rows = 12
row_rule_len = 3
row_rules = [[0, 0, 2], [0, 1, 2], [0, 1, 1], [0, 0, 2], [0, 0, 1], [0, 0, 3],
             [0, 0, 3], [0, 2, 2], [0, 2, 1], [2, 2, 1], [0, 2, 3], [0, 2, 2]]

cols = 10
col_rule_len = 2
col_rules = [[2, 1], [1, 3], [2, 4], [3, 4], [0, 4], [0, 3], [0, 3], [0, 3],
             [0, 2], [0, 2]]

if __name__ == '__main__':
  if len(sys.argv) > 1:
    file = sys.argv[1]
    exec(compile(open(file).read(), file, 'exec'))
  main(rows, row_rule_len, row_rules, cols, col_rule_len, col_rules)