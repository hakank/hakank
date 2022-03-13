#
# Argmax in z3
#
# Find the index of the largest value in an array.
# 
# See seven_cups.py for a simple example of this.
#
# Note: z3_utils_hakank.py also defines argmin()
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *


def argmax_test(n=7,total=100,use_real=True):

  s = SimpleSolver()
  # s = Solver()
  # s = SolverFor("QF_NRA")
  # s = Optimize()

  # The values
  if use_real:
      x = [Real(f"x[{i}]") for i in range(n)]
      max_val = Real("max_val")
  else:
      x = [Int(f"x[{i}]") for i in range(n)]
      max_val = Int("max_val")

  s.add(Sum(x) == total)

  # The index of the largest value
  ix = Int("ix")
  for i in range(n):
      s.add(ix >= 0, ix < n)

  s.add(Distinct(x))
  # s.add(Not(Distinct(x)))

  argmax(s,x,ix,max_val)

  num_solutions = 0
  while s.check() == sat:
    num_solutions += 1
    mod = s.model()
    if use_real:
        print("ix:", mod[ix], "max_val:", mod[max_val].as_decimal(6))    
        print("x :", [mod.eval(x[i]).as_decimal(6) for i in range(n)])
    else:
        print("ix:", mod[ix], "max_val:", mod[max_val])    
        print("x :", [mod.eval(x[i]) for i in range(n)])
        
    print()
    s.add(ix != mod[ix])
    # s.add(Or([x[i] != mod[x[i]] for i in range(n)]))

  print("num_solutions:", num_solutions)


n = 7
total = 100
print("Testing Reals")
use_real = True
argmax_test(n,total,use_real)

print("\nTesting Ints")
argmax_test(n,total,False)
