"""
  Nonogram (Painting by numbers) in cpmpy.

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


  I have blogged about the development of a Nonogram solver in Comet
  using the regular constraint.
  * 'Comet: Nonogram improved: solving problem P200 from 1:30 minutes
     to about 1 second'
     http://www.hakank.org/constraint_programming_blog/2009/03/comet_nonogram_improved_solvin_1.html

  * 'Comet: regular constraint, a much faster Nonogram with the regular
    constraint, some OPL models, and more'
     http://www.hakank.org/constraint_programming_blog/2009/02/comet_regular_constraint_a_muc_1.html

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys, math
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

class ORT_nonogram_printer(ort.CpSolverSolutionCallback):
  """
  A simple printer callback for printing a matrix.
  """
  def __init__(self, varmap, board, rows,cols, num_solutions=0):
    super().__init__()

    self.solcount = 0
    self.varmap = varmap
    self.vars = (board)
    self.rows = rows
    self.cols = cols
    self.num_solutions=num_solutions

  def on_solution_callback(self):
    self.solcount += 1

    for cpm_var in self.vars:
      cpm_var._value = self.Value(self.varmap[cpm_var])

    (board) = self.vars
    print(f"#{self.solcount}:")

    for i in range(self.rows):
        row = [board[i*cols+ j].value() - 1 for j in range(cols)]
        row_pres = []
        for j in row:
            if j == 1:
                row_pres.append('#')
            else:
                row_pres.append(' ')
        print('  ', ''.join(row_pres))

    print(flush=True)        
    print('  ', '-' * cols)

    if self.num_solutions > 0 and self.solcount >= self.num_solutions:
      self.StopSearch()

#
# Make a transition (automaton) matrix from a
# single pattern, e.g. [3,2,1]
#
def make_transition_matrix(pattern):
    p_len = len(pattern)
    num_states = p_len + sum(pattern)

    # this is for handling 0-clues. It generates
    # just the state 1,2
    if num_states == 0:
        num_states = 1

    t_matrix = []
    for i in range(num_states):
        row = []
        for j in range(2):
            row.append(0)
        t_matrix.append(row)

    # convert pattern to a 0/1 pattern for easy handling of
    # the states
    tmp = [0 for i in range(num_states)]
    c = 0
    tmp[c] = 0
    for i in range(p_len):
        for j in range(pattern[i]):
            c += 1
            tmp[c] = 1
        if c < num_states - 1:
            c += 1
            tmp[c] = 0

    t_matrix[num_states - 1][0] = num_states
    t_matrix[num_states - 1][1] = 0

    for i in range(num_states):
        if tmp[i] == 0:
            t_matrix[i][0] = i + 1
            t_matrix[i][1] = i + 2
        else:
            if i < num_states - 1:
                if tmp[i + 1] == 1:
                    t_matrix[i][0] = 0
                    t_matrix[i][1] = i + 2
                else:
                    t_matrix[i][0] = i + 2
                    t_matrix[i][1] = 0

    # print('The states:') # For debugging
    # for i in range(num_states):
    #     for j in range(2):
    #         print(t_matrix[i][j],end=" ")
    #     print()
    # print()

    return t_matrix


#
# check each rule by creating an automaton
# and regular
#
def check_rule(rules, y):
    r_len = sum([1 for i in range(len(rules)) if rules[i] > 0])
    rules_tmp = []
    for i in range(len(rules)):
        if rules[i] > 0:
            rules_tmp.append(rules[i])
    transition_fn = make_transition_matrix(rules_tmp)
    n_states = len(transition_fn)
    input_max = 2

    # Note: we cannot use 0 since it's the failing state
    initial_state = 1
    accepting_states = [n_states]  # This is the last state

    return [regular(y, n_states, input_max, transition_fn, initial_state,
                    accepting_states)]


def nonogram_regular(rows, row_rule_len, row_rules, cols, col_rule_len, col_rules,num_sols=2):

    #
    # variables
    #
    board = intvar(1,2,shape=(rows,cols),name="board")
    board_flat = flatten_lists(board)

    model = Model()

    #
    # constraints
    #
    for i in range(rows):
        model += [check_rule([row_rules[i][j] for j in range(row_rule_len)],
                             [board[i, j] for j in range(cols)])]

    for j in range(cols):
        model += [check_rule([col_rules[j][k] for k in range(col_rule_len)],
                             [board[i, j] for i in range(rows)])]


    print("Solve")

    # all solution solving, with blocking clauses
    ss = CPM_ortools(model)    

    cb = ORT_nonogram_printer(ss.varmap,board_flat,rows,cols,num_sols)

    # Flags to experiment with
    if num_sols == 1:
        ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    ss.ort_solver.parameters.cp_model_presolve = False
    ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0

    ort_status = ss.ort_solver.SearchForAllSolutions(ss.ort_model, cb)
    print(ss._after_solve(ort_status)) # post-process after solve() call...
    print(ss.status())
    print("Nr solutions:", cb.solcount)
    print("Num conflicts:", ss.ort_solver.NumConflicts())
    print("NumBranches:", ss.ort_solver.NumBranches())
    print("WallTime:", ss.ort_solver.WallTime())


#
# Default problem
#
# From http://twan.home.fmf.nl/blog/haskell/Nonograms.details
# The lambda picture
#
# See http://hakank.org/cpmpy/ (nonogram_*.py) for more Nonogram instances.
# 
rows = 12
row_rule_len = 3
row_rules = [[0, 0, 2], [0, 1, 2], [0, 1, 1], [0, 0, 2], [0, 0, 1], [0, 0, 3],
             [0, 0, 3], [0, 2, 2], [0, 2, 1], [2, 2, 1], [0, 2, 3], [0, 2, 2]]

cols = 10
col_rule_len = 2
col_rules = [[2, 1], [1, 3], [2, 4], [3, 4], [0, 4], [0, 3], [0, 3], [0, 3],
             [0, 2], [0, 2]]

num_sols = 2
if len(sys.argv) > 1:
    file = sys.argv[1]
    exec(compile(open(file).read(), file, 'exec'))
nonogram_regular(rows, row_rule_len, row_rules, cols, col_rule_len, col_rules,num_sols)