"""
Divisible by 9 through 1 puzzle in CPMpy.

From http://msdn.microsoft.com/en-us/vcsharp/ee957404.aspx
'Solving Combinatory Problems with LINQ'
'''
Find a number consisting of 9 digits in which each of the digits
from 1 to 9 appears only once. This number must also satisfy these
divisibility requirements:
  1. The number should be divisible by 9.
  2. If the rightmost digit is removed, the remaining number should
      be divisible by 8.
  3. If the rightmost digit of the new number is removed, the remaining
      number should be divisible by 7.
  4. And so on, until there's only one digit (which will necessarily
     be divisible by 1).
'''
Also, see 'Intel Parallel Studio: Great for Serial Code Too (Episode 1)'
http://software.intel.com/en-us/blogs/2009/12/07/intel-parallel-studio-great-for-serial-code-too-episode-1/

Model created by Hakan Kjellerstrand, hakank@gmail.com
See also my CPMpy page: http://www.hakank.org/cpmpy/
"""

from cpmpy import *
import numpy as np
from cpmpy_hakank import *

def divisible_by_9_to_1(base=10,print_model=False):
  
  # data
  m = base ** (base - 1) - 1
  n = base - 1
  digits_str = "0123456789ABCDEFGH"

  print("base:", base,"n:",n, "m:",m,"print_model:",print_model)

  # declare variables
  x = IntVar(1,n, shape=n,name="x")

  # the numbers where t[n] contains the answer
  t = IntVar(0,m, shape=n,name="t")

  # Constraints
  model = Model([AllDifferent(x),
                 AllDifferent(t),
            ])

  for i in range(n):
    mm = base-i-1
    model += [to_num([x[j] for j in range(mm)], t[i], base)]
    model += [(t[i] % mm) == 0]

  if print_model:
    print("model:", model)

  def print_sol():
    xx = x.value()
    tt = t.value()
    print("x: ", xx)
    print("t: ", tt)
    print("number base 10: %i base %i: %s" % (tt[0],
                                              base,
                                              "".join([digits_str[xx[i]] for i in range(n)])))

  ss = CPM_ortools(model)
  ss.ort_solver.parameters.linearization_level = 0
  ss.ort_solver.parameters.cp_model_probing_level = 0  
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)

# base 16 is the maximum allowed value
for base in range(3,16+1):
  divisible_by_9_to_1(base)
  print()

# Print the model as well
divisible_by_9_to_1(10,True)
