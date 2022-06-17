"""
Word square in cpmpy.

From http://en.wikipedia.org/wiki/Word_square
'''
A word square is a special case of acrostic. It consists of a set of words, 
all having the same number of letters as the total number of words (the 
"order" of the square); when the words are written out in a square grid 
horizontally, the same set of words can be read vertically.
'''

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my CPMpy page: http://www.hakank.org/cpmpy/

"""
import sys
import re
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


class ORT_word_square_printer(ort.CpSolverSolutionCallback):
  """
  A simple printer callback for single array printing.
  """
  def __init__(self, varmap, a, num_solutions=0):
    super().__init__()
    self.solcount = 0
    self.varmap = varmap
    self.vars = (a)
    self.num_solutions=num_solutions

  def on_solution_callback(self):
    self.solcount += 1

    # For single arrays:
    for cpm_var in self.vars:
      cpm_var._value = self.Value(self.varmap[cpm_var])
      
    (a) = self.vars            
    print(f"#{self.solcount}")
    for w in a:
        print(words[w.value()])
    print(flush=True)

    if self.num_solutions > 0 and self.solcount >= self.num_solutions:
      self.StopSearch()

#
# convert a character to integer
#
def get_dict():
    alpha = "abcdefghijklmnopqrstuvwxyzåäö";
    d = {}
    rev = {}
    count = 1
    for a in alpha:
        d[a] = count
        rev[count] = a
        count += 1
    return d, rev
    

def word_square(words, word_len, num_sols=20,num_procs=1):

    num_words = len(words)
    n = word_len
    d, rev = get_dict()
    
    A = intvar(0,29,shape=(num_words, word_len),name="A")
    E = intvar(0, num_words,shape=n,name="E")

    model = Model(
                   AllDifferent(E) 
                   )

    # copy the words to a matrix
    for i in range(num_words):
        for j in range(word_len):
            model += [A[i,j] == d[words[i][j]]]

    # connect the words by character in proper positions
    for i in range(word_len):
        for j in range(word_len):
            model += [A[E[i],j] == A[E[j],i]]

    ss = CPM_ortools(model)
    # Note that we have to use a flattened version of x.
    cb = ORT_word_square_printer(ss._varmap,E,num_sols)

    if num_procs > 1:
        print("number of processes:", num_procs)
        ss.ort_solver.parameters.num_search_workers = num_procs

    # Flags to experiment with        
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    # ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0

    if num_sols == 1:
        ort_status = ss.ort_solver.Solve(ss.ort_model, cb)
    else:
        ort_status = ss.ort_solver.SearchForAllSolutions(ss.ort_model, cb)


    # print("After solve status:", ss._after_solve(ort_status)) # post-process after solve() call...
    print("s.status():", ss.status())
    print("Nr solutions:", cb.solcount)
    print("Num conflicts:", ss.ort_solver.NumConflicts())
    print("NumBranches:", ss.ort_solver.NumBranches())
    print("WallTime:", ss.ort_solver.WallTime())
    
    return ss.ort_solver.WallTime()


def print_solution(E):
    print(E.value())
    for e in E:
        print(words[e.value()])
    

def read_words(word_list, word_len, limit):
    dict = {}
    all_words = []
    count = 0
    words = open(word_list).readlines()
    for w in words:
        w = w.strip().lower()
        if len(w) == word_len and not w in dict and not re.search("[^a-zåäö]",w):
            dict[w] = 1
            all_words.append(w)
            count += 1
    return all_words


word_dict = "/usr/share/dict/words"
# word_dict = "words_lower.txt"
word_len = 5
limit = 1000000
num_sols = 3
if len(sys.argv) > 1:
    word_dict = sys.argv[1]
if len(sys.argv) > 2:
    word_len = int(sys.argv[2])
if len(sys.argv) > 3:
    limit = int(sys.argv[3])
if len(sys.argv) > 4:
    num_sols = int(sys.argv[4])

# Note: I have to use a limit, otherwise it seg faults
words = read_words(word_dict, word_len, limit)
print("It was", len(words), "words")
word_square(words, word_len,num_sols)
