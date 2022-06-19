"""
Hamming distance in cpmpy.

I.e. the number of bits differing in two (binary) arrays.
See http://en.wikipedia.org/wiki/Hamming_distance

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def hamming_distance(a, b, d):
  # return (d == sum([a[i] != b[i] for i in range(len(a))]))
  return (d == sum(a != b))

def hamming_distance2(a, b):
  # return (d == sum([a[i] != b[i] for i in range(len(a))]))
  return (sum(a != b))


def hamming_distance_model(a_given="",n=6):

  model = Model()

  a = boolvar(shape=n,name="a")
  b = boolvar(shape=n,name="b")
  diffs = intvar(0,n,name="diffs") # The number of differences 

  if a_given != "":
    print("a_given:", a_given)
    n = len(a_given)
  
    for i in range(n):
      model += (a[i] == a_given[i])
  else:
    # a < b
    model += [lex_less(a,b)]

  #
  # We can now either
  # - Calculate the hamming distance from two arrays
  # - Given the distance, generate all arrays which has the hamming distance
  #
  # model += [hamming_distance(a, b, diffs)]
  model += [diffs == hamming_distance2(a, b)]
  
  # model += (diffs == 2)
  
  def print_sol():
    print("a:", a.value())
    print("b:", b.value())    
    print("diffs:", diffs.value())
    print()

  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)


a_given = [1,1,1,1,0,0]
n=len(a_given)
hamming_distance_model(a_given,n)
print("all a and b of size 4 where a < b (lex_less(a,b)):")
hamming_distance_model("",4)



