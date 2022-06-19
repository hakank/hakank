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


    def print_sol():
      for w in E:
        print(words[w.value()])
      print(flush=True)

    ss = CPM_ortools(model)
    # Flags to experiment with        
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0

    num_solutions = ss.solveAll(solution_limit=num_sols,display=print_sol)
    print("Nr solutions:", num_solutions)
    print("Num conflicts:", ss.ort_solver.NumConflicts())
    print("NumBranches:", ss.ort_solver.NumBranches())
    print("WallTime:", ss.ort_solver.WallTime())
    
    return ss.ort_solver.WallTime()


def read_words(word_list, word_len):
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
num_sols = 3
if len(sys.argv) > 1:
    word_dict = sys.argv[1]
if len(sys.argv) > 2:
    word_len = int(sys.argv[2])
if len(sys.argv) > 3:
    limit = int(sys.argv[3])
if len(sys.argv) > 4:
    num_sols = int(sys.argv[4])

words = read_words(word_dict, word_len)
print("It was", len(words), "words")
word_square(words, word_len,num_sols)
