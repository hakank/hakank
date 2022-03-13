#
# Seven cups in z3
#
# https://python.tutorialink.com/finding-max-of-the-numbers-in-z3-using-smtlib2/
# """
# I have 7 cups which contains some water. I need to program these cups to have
# different amounts of water. Once this is done I need to measure the cup which
# has the highest amount of water and then remove some quantity (say 2 units of
# water).
# """
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *


def seven_cups(n=7,total=100,to_remove=2):

  # s = SimpleSolver()
  # s = Solver()
  s = SolverFor("QF_NRA")
  # s = Optimize()

  # Original values
  x = [Real(f"x[{i}]") for i in range(n)]
  for i in range(n):
      s.add(x[i] >= to_remove)

  # hakank: Let's add some more constraints
  s.add(Sum(x) == total)

  # Copy of x except that the largest value is subtracted by 2
  x2 = [Real(f"x2[{i}]") for i in range(n)]

  # The index of the largest value
  ix = Int("ix")
  for i in range(n):
      s.add(ix >= 0, ix < n)

  max_val = Real("max_val")

  s.add(Distinct(x))
  # s.add(Not(Distinct(x)))

  argmax(s,x,ix,max_val)
  for i in range(n):
      # Subtract 2 from the largest number (and keeo the other values intact)
      s.add(If(ix == i, x2[i] == x[i]-to_remove, x2[i] == x[i]))

  num_solutions = 0
  while s.check() == sat:
    num_solutions += 1
    mod = s.model()
    print("ix:", mod[ix], "max_val:", mod[max_val].as_decimal(6))    
    print("x :", [mod.eval(x[i]).as_decimal(6) for i in range(n)])
    print("x2:", [mod.eval(x2[i]).as_decimal(6) for i in range(n)])    
    print()
    s.add(ix != mod[ix])
    # s.add(Or([x[i] != mod[x[i]] for i in range(n)]))

  print("num_solutions:", num_solutions)


n = 7
total = 100
to_remove = 2
seven_cups(n,total,to_remove)
