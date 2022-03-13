# 
# Rosenbrock function (a nonlinear standard problem) in Z3
#
# This is problem 3.1 from
# http://www.cs.cas.cz/ics/reports/v798-00.ps
# """
# converted to AMPL by Yu-Ju Kuo and Hans D. Mittelmann
# """
#
# Also see:
# http://mathworld.wolfram.com/RosenbrockFunction.html
# http://en.wikipedia.org/wiki/Rosenbrock_function
# """
# It is also known as Rosenbrock's valley or Rosenbrock's banana function.
# It has a global minimum at (x,y) = (1,1) where f(x,y) = 0.
# """
#  
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from z3 import *


def rosenbrock():

  # s = Optimize() # too slow
  # s = SimpleSolver()
  s = Solver()

  # data

  x1,x2,z = Reals("x1 x2 z")
  # s.add(z == 100.0*(x2-x1*x1)*(x2-x1*x1)+(1.0-x1)*(1.0-x1))
  s.add(z == 100.0*(x2-x1**2)**2+(1.0-x1)**2) # faster
  # s.minimize(z)
  
  num_solutions = 0
  while s.check() == sat:
    num_solutions += 1
    mod = s.model()
    print("x1:", mod[x1].as_decimal(6), "x2:", mod[x2].as_decimal(6), "z:", mod[z].as_decimal(6))
    s.add(z < mod[z]-0.0001)
    

  print("num_solutions:", num_solutions)


rosenbrock()
