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

  Word square in OR-tools CP-SAT Solver.

  From http://en.wikipedia.org/wiki/Word_square
  '''
  A word square is a special case of acrostic. It consists of a set of words,
  all having the same number of letters as the total number of words (the
  'order' of the square); when the words are written out in a square grid
  horizontally, the same set of words can be read vertically.
  '''

  This is a port of my old CP model word_square.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import sys, re
# from cp_sat_utils import *

class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, E, words, num_answers):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__E = E
        self.__words = words
        self.__num_answers = num_answers
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        print_solution(self, self.__E, self.__words)
        if self.__num_answers > 0 and self.__solution_count >= self.__num_answers:
          self.StopSearch()

    def SolutionCount(self):
        return self.__solution_count


def main(words, word_len, num_answers=5):

  model = cp.CpModel()

  #
  # data
  #
  num_words = len(words)
  n = word_len
  d, _rev = get_dict()

  #
  # declare variables
  #
  A = {}
  for i in range(num_words):
    for j in range(word_len):
      A[(i, j)] = model.NewIntVar(0, 29, "A(%i,%i)" % (i, j))

  A_flat = [A[(i, j)] for i in range(num_words) for j in range(word_len)]

  E = [model.NewIntVar(0, num_words, "E%i" % i) for i in range(n)]

  #
  # constraints
  #
  model.AddAllDifferent(E)

  # copy the words to a Matrix
  for I in range(num_words):
    for J in range(word_len):
      model.Add(A[(I, J)] == d[words[I][J]])

  for i in range(word_len):
    for j in range(word_len):
      # This is what I would like to do:
      # solver.Add(A[(E[i],j)] == A[(E[j],i)])

      # We must use Element explicitly
      val = model.NewIntVar(0,29,"t")
      ix1 = model.NewIntVar(0,num_words,"t")
      model.Add(ix1 == E[i] * word_len + j) 
      ix2 = model.NewIntVar(0,num_words,"t")
      model.Add(ix2 == E[j] * word_len + i) 
      model.AddElement(ix1, A_flat, val)
      model.AddElement(ix2, A_flat, val)
  #
  # solution and search
  #
  solver = cp.CpSolver()
  # solver.parameters.search_branching = cp.PORTFOLIO_SEARCH
  # solver.parameters.cp_model_presolve = False
  solver.parameters.linearization_level = 0
  solver.parameters.cp_model_probing_level = 0

  solution_printer = SolutionPrinter(E,words, num_answers)
  status = solver.SearchForAllSolutions(model, solution_printer)

  if not (status == cp.OPTIMAL or status == cp.FEASIBLE):
    print("No solution!")


  print()
  print("NumSolutions:", solution_printer.SolutionCount())
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


#
# convert a character to integer
#
def get_dict():
  alpha = "abcdefghijklmnopqrstuvwxyzåäö"
  d = {}
  rev = {}
  count = 1
  for a in alpha:
    d[a] = count
    rev[count] = a
    count += 1
  return d, rev


def print_solution(solver, E, words):
  # print E
  for e in E:
    print(words[solver.Value(e)])
  print()


def read_words(word_list, word_len, limit):
  dict = {}
  all_words = []
  count = 0
  words = open(word_list).readlines()
  for w in words:
    w = w.strip().lower()
    if len(w) == word_len and w not in dict and not re.search("[^a-zåäö]", w):
      dict[w] = 1
      all_words.append(w)
      count += 1
  return all_words


# word_dict = "examples/data/words/list.txt"
word_dict = "list.txt"
word_len = 4
limit = 1000000
num_answers = 5

if __name__ == "__main__":

  if len(sys.argv) > 1:
    word_dict = sys.argv[1]
  if len(sys.argv) > 2:
    word_len = int(sys.argv[2])
  if len(sys.argv) > 3:
    limit = int(sys.argv[3])
  if len(sys.argv) > 4:
    num_answers = int(sys.argv[4])

  # Note: I have to use a limit, otherwise it seg faults
  words = read_words(word_dict, word_len, limit)
  print("It was", len(words), "words")
  main(words, word_len, num_answers)
