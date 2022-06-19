"""
Polydivisible numbers in cpmpy.

What is the largest polydivisible number such that all subsequences of the number (1..n)
is divisible by n?

See the Picat forum for a discussion:
https://groups.google.com/forum/#!topic/picat-lang/Cb3RWZ6h-LE

Note: cpmpy (OR-tools CP-SAT) cannot handle integer larger than 16 digits.

Number of polydivisible numbers of a certain length (see go3/0):
 N  #solutions
 -----------
 1 = 9
 2 = 45
 3 = 150
 4 = 375
 5 = 750
 6 = 1200
 7 = 1713
 8 = 2227
 9 = 2492
 10 = 2492
 11 = 2225
 12 = 2041
 13 = 1575
 14 = 1132
 15 = 770
 16 = 571
 17 = 335
 18 = 180
 19 = 90
 20 = 44
 21 = 18
 22 = 12
 23 = 6
 24 = 3
 25 = 1
 26 = 0

Also see:
* https://en.wikipedia.org/wiki/Polydivisible_number
* http://www.blog.republicofmath.com/the-number-3608528850368400786036725/
#

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *



def polydivisible_numbers(n=10):

  model = Model()

  # data
  m = 10 ** (n)-1
  print("n:", n,"m:",m)

  # declare variables

  # the digits
  x = intvar(0,9,shape=n ,name="x")
  # the numbers where t[n-1] contains the answer
  t = intvar(1,m,shape=n,name="t")
      
  for i in range(1,n+1):
    model += (to_num([x[j] for j in range(i)], t[i-1], n))
    model += (t[i-1] % i == 0) 

  def print_sol():
    print("x:",x.value())
    print("t:",t.value())
    print()
    
  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:",num_solutions)


base = 10
if len(sys.argv) > 1:
    base = int(sys.argv[1])
polydivisible_numbers(base)

# for base in range(2, 26):
#     polydivisible_numbers(base)
# print()
