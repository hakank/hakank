"""
Recover origin of pair differences in cpmpy.

From https://twitter.com/SturnioloSimone/status/1214588457135329280
'''
Suppose there's a certain set of numbers - x_1, x_2, ..., x_n. You 
don't know n. Suppose you only know certain linear combinations of them, 
L_ij = x_i-x_j, but you DON'T know i or j. What's an algorithm to find 
as many of the xs as possible just from looking at the Ls? Ideas?

... 
(a follow up tweet:)
There are four numbers. I can tell you that 1, 3, and 6 are differences 
between pairs of numbers chosen among them (you don't know which pairs). 
Can you tell me what the numbers are?

Something like that, except you don't know it's four.
'''

This is a port of my OR-tools CP-SAT model which is a port of
my Picat model http://hakank.org/picat/linear_combinations2.pi 


Here's a summary of the problem.

Let's assume that we start with the list 
  l = [1,4,8,9].
The unique and sorted pairs of differences is 
  d = [1, 3, 4, 5, 7, 8]
Now, given the list d, we are trying to recover the 
original list l.

Also, the method only give shifted solutions, i.e.
if the original list is [10,11,13,14] we recover it
as the shifted list:
  [1,2,3,4]

(Personal note: The shifted list might start at 0, i.e. 
 [0,1,2,3] in the example above, but it's more naturally 
  for me to start with 1. Also the original model was written
  in Picat which is 1-based.)

Note that the recovery is _not_unique: there might be more than
one solution - even a huge number solutions - to the problem.
Example: For the original list of 
  [1,2,3,4,5,6,7]
There are 21 possible solutions, all with the same 
difference list of [1, 2, 3, 4, 5, 6] (duplicates are removed):

  x_restored: [1, 3, 4, 5, 7]
  x_restored: [1, 4, 6, 7]
  x_restored: [1, 3, 6, 7]
  x_restored: [1, 3, 4, 6, 7]
  x_restored: [1, 2, 4, 7]
  x_restored: [1, 2, 3, 4, 7]
  x_restored: [1, 2, 4, 6, 7]
  x_restored: [1, 2, 3, 6, 7]
  x_restored: [1, 2, 3, 4, 6, 7]
  x_restored: [1, 4, 5, 7]
  x_restored: [1, 4, 5, 6, 7]
  x_restored: [1, 3, 4, 5, 6, 7]
  x_restored: [1, 3, 5, 6, 7]
  x_restored: [1, 2, 3, 5, 7]
  x_restored: [1, 2, 3, 4, 5, 7]
  x_restored: [1, 2, 4, 5, 7]
  x_restored: [1, 2, 5, 7]
  x_restored: [1, 2, 5, 6, 7]
  x_restored: [1, 2, 4, 5, 6, 7]
  x_restored: [1, 2, 3, 4, 5, 6, 7]
  x_restored: [1, 2, 3, 5, 6, 7]

See the following for a more thorough description of the
problem and discussion of many solutions etc:
http://hakank.org/picat/Recover_origin_of_pair_differences.pdf

Note: much of the runtime are the two large loops.


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def unique(l):
  """
  Return the unique values of list l.
  """
  return list(dict.fromkeys(l))

def diffs(l):
  """
  'Simple' (i.e. not pairs) differences of a list l.
  """
  d = [l[i]-l[i-1] for i in range(1,len(l))]
  d = list(dict.fromkeys(d))
  d = sorted(d)
  return d

def shifted_list(l):
  """
  Return the shifted (normalized) list of list l.
  A shifted/normalized is a list which starts with 0 and ends with 
  last(l)-l[0]
  """
  return [l[i] - l[0]+1 for i in range(len(l))]

def pair_differences(l):
  """
  Return he (sorted) pair differences of the list l.
  """
  n = len(l)
  p = sorted(unique([abs(l[i]-l[j]) for i in range(n) for j in range(i+1,n)]))
  return p


def min_len(l_len):
  """
  (Theoretical) minumum length of a origin list given a difference 
  list of length Len .
  """
  return math.floor(1+math.sqrt(1+8*l_len)/2)

def linear_combinations(ds=[1,2,3],num_sols = 0):

  model = Model()

  #
  # Prepare the data
  #

  # The maximum element in the (shifted) solution
  # is the maximum value in the difference list + 1
  n = max(ds) + 1 

  # We compensate for Python's 0-baseness
  # by ignoring index 0 and thus add one element
  # in the list
  n1 = n+1 
  print("n:",n,"n1:",n1,"size of the matrix:", n1*n1)

  x = boolvar(shape=n1,name="x")
  z = intvar(1,n,name="z")

  # Symmetry breaking: 1 is the first number.
  model += [x[0] == 0] # Skip the 0 coeff
  model += [x[1] == 1]

  # Since we start with 1, the last (and maximum) number in x is
  # max(diffs) + 1 (which is the max domain value as well)
  model += [x[n] == 1]

  model += [z == sum(x)]

  # Ensure that we cover all the differences (in diffs),
  # i.e. find some i and j (i<j) where abs(x[i]-x[j]) == d
  for d in ds:
    model += [sum([j*x[j]-i*x[i] == d for i in range(1,n1) for j in range(i+1,n1)]) > 0]
    
  # All the differences from the pairs in the list x must 
  # be in the Diff list
  for i in range(1,n1):
    for j in range(i+1,n1):
      ## x[i] + x[j] == 2 => sum([d==abs(i-j) : d in diffs]) > 0  # Picat

      # This don't work since the RHS is not a proper type
      # model += [ (x[i] & x[j]).implies(sum([d==abs(i-j) for d in ds]) > 0)]

      # This works but is slow
      # model += [ (x[i] & x[j]).implies(sum([d==my_abs2(i,j) for d in ds]) > 0) ]
      
      # This works (trying to force the proper abs() function...
      # But it's slow...
      # ii = intvar(i,i)
      # jj = intvar(j,j)    
      ## model += [ (x[i] & x[j]).implies(sum([d==abs(ii-jj) for d in ds]) > 0) ]
      # model += [ (x[i] & x[j])<=(sum([d==abs(ii-jj) for d in ds]) > 0) ]
      
      # This works and is faster
      # model += [ (x[i] & x[j])<=(sum([d==abs(i-j) for d in ds]) > 0) ]
      model += [ (x[i] + x[j] == 2) <= int(sum([d==abs(i-j) for d in ds]) > 0) ]

  ## model.minimize(z)

  print("solve")

  x_restored_a = []
  def print_sol():
    xval = x.value()
    # print("x:", xval)
    print("z:", z.value())
    x_restored = [i for i in range(n1) if xval[i] == 1]
    print("x_restored:",x_restored)
    x_restored_a.append(x_restored)
    diffs_restored = pair_differences(x_restored)
    print("diffs(x_restored):", diffs_restored )
    
  
  ss = CPM_ortools(model)
  # ss.ort_solver.parameters.log_search_progress = True
  # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
  # ss.ort_solver.parameters.cp_model_presolve = False
  ss.ort_solver.parameters.linearization_level = 0
  ss.ort_solver.parameters.cp_model_probing_level = 0

  num_solutions = ss.solveAll(solution_limit=num_sols,display=print_sol)
  print("num_solutions:", num_solutions)
  print("WallTime:", ss.ort_solver.WallTime())
  print("NumBranches:", ss.ort_solver.NumBranches())
  print("NumConflicts:", ss.ort_solver.NumConflicts())
  print("Stats:", ss.ort_solver.ResponseStats())
  print()

  return x_restored_a

#
# Some instances:
#
l = [5,14,18,34,38,49]
# l = [1,2,3,4,5]
# l = [1,3,8]
# l = [16,41,43,54,56,62,66,67,84,95] # Total 53.5s solve time: 2.76s

# There's a lot of solutions.
# Time for 1st solution: Total: 19.3s runtime, solve time: 2.47s
# l = [i for i in range(1,48)]

# This is slow.
# - Time for first solution: Total time 148.7s of which 13.2s is the solve time.
# . Time for both solutions: Total time: 177,81s
# l = [40, 108, 122, 136, 141, 143, 146, 188, 192] 
print("l:",l)
shifted = shifted_list(l)
print("shifted:", shifted)
ds = pair_differences(l) # diffs(l)
print("diffs:",ds)

print()
num_sols = 1
l_restored_a = linear_combinations(ds,num_sols)

#
# Did we restored the list?
#
for l_restored in l_restored_a:
  if l_restored == shifted:
    print("shifted is restored!")

print()

