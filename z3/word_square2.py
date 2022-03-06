#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Word square in Z3
#
# From http://en.wikipedia.org/wiki/Word_square
# '''
# A word square is a special case of acrostic. It consists of a set of words,
# all having the same number of letters as the total number of words (the
# 'order' of the square); when the words are written out in a square grid
# horizontally, the same set of words can be read vertically.
# '''
#
# This is a variant of word_square.py with the difference that it don't
# use an Array to handle the element constraints, but the explicit
# element constraint.
#
# It takes longer to set up all the constraints, but the solving time
# is significantly faster.
#
# Comparison with word_square.py
#
#  word_len   num_answers      time word_square.py  time word_square.2py
#  -----------------------------------------------------------------------
#  3          20                    16.8s                5.9s
#  4          20               32min17,40s              54.2s
#
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from __future__ import print_function
import sys
import re
from z3_utils_hakank import *

def main(words, word_len, num_answers=20):

  sol = SimpleSolver() # word_len 3 and 20 answers: 6.1s   word_len 4 and 20 answers: 54,2s
  # sol = SolverFor("QF_LIA") # # word_len 3 and 20 answers: 6.1s  word_len 4 and 20 answers_ 56,467
  
  # data
  num_words = len(words)
  n = word_len
  d, rev = get_dict()

  #
  # declare variables
  #
  A = {}
  for i in range(num_words):
    for j in range(word_len):
      A[(i, j)] = Int(f"A[{i},{j}")
      sol.add(A[(i, j)] >= 0, A[(i,j)] <= 29)
  A_flat = [A[(i,j)] for i in range(num_words) for j in range(word_len)]
  E = makeIntVector(sol,"E", n, 0,num_words-1)

  #
  # constraints
  #
  sol.add(Distinct(E))

  # copy the words to a Matrix
  for I in range(num_words):
    for J in range(word_len):
      sol.add(A[(I, J)] == d[words[I][J]])

  for i in range(word_len):
    for j in range(word_len):
      AEij = Int(f"AEij[{i}.{j}")
      sol.add(AEij >= 0)
      element(sol,E[i]*word_len+j,A_flat,AEij,num_words*word_len)
      element(sol,E[j]*word_len+i,A_flat,AEij,num_words*word_len)      

  # solution
  print("solve")
  num_solutions = 0
  while sol.check() == sat:
    num_solutions += 1
    mod = sol.model()
    print_solution(mod, E, words)
    if num_solutions >= num_answers:
      break
    else:
      getDifferentSolution(sol,mod,E)

  print()
  print("num_solutions:", num_solutions)
  


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


def print_solution(mod, E, words):
  # print(E)
  for e in E:
    print(words[mod.eval(e).as_long()])
  print()


def read_words(word_list, word_len, limit):
  dict = {}
  all_words = []
  count = 0
  words = open(word_list).readlines()
  for w in words:
    w = w.strip().lower()
    if len(w) == word_len and w not in dict and not re.search(
        "[^a-zåäö]", w):
      dict[w] = 1
      all_words.append(w)
      count += 1
      if count >= limit:
        break
  return all_words


word_dict = "/usr/share/dict/words"
word_len = 4
limit = 1000000
num_answers = 20

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
