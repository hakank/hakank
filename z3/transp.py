# 
# Transportation problem in Z3
#
# From GLPK:s example transp.mod
# """
# A TRANSPORTATION PROBLEM
# 
# This problem finds a least cost shipping schedule that meets
# requirements at markets and supplies at factories.
# 
# References:
#              Dantzig G B, "Linear Programming and Extensions."
#              Princeton University Press, Princeton, New Jersey, 1963,
#              Chapter 3-3.
# """
#
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from z3 import *


def transp():

  s = Optimize() 
  # s = SimpleSolver()
  # s = Solver()

  #
  # data
  #
  # plants 
  # Seattle San-Diego
  plants = ["Seattle", "San-Diego"]
  num_plants = len(plants)

  # markets 
  markets=["New-York", "Chicago", "Topeka"]
  num_markets = len(markets)

  #  capacity of plant i in cases 
  a = [350.0, 600.0]
  
  # demand at market j in cases 
  b = [325.0, 300.0, 275.0]
  
  # distance in thousands of miles 
  d = [[2.5, 1.7, 1.8],
       [2.5, 1.8, 1.4]]

  # freight in dollars per case per thousand miles 
  f = 90.0

  # transport cost in thousands of dollars per case 
  c = [ [f*d[i][j] / 1000.0 for j in range(num_markets) ] for i in range(num_plants)]

  #
  # decision variables
  #

  # shipment quantities in cases 
  x = [ [Real(f"x[{i,j}") for j in range(num_markets) ] for i in range(num_plants)]
  for i in range(num_plants):
    for j in range(num_markets):
      s.add(x[i][j] >= 0)

  # total transportation costs in thousands of dollars 
  cost = Real("cost")
  s.add(cost == Sum([c[i][j]*x[i][j] for i in range(num_plants) for j in range(num_markets)]))


  # observe supply limit at plant i 
  for i in range(num_plants):
      s.add(Sum([x[i][j] for j in range(num_markets)]) <= a[i])

  # satisfy demand at market j 
  for j in range(num_markets):
    s.add(Sum([x[i][j] for i in range(num_plants)]) >= b[j])

  s.minimize(cost)

  num_solutions = 0
  if s.check() == sat:
    num_solutions += 1
    mod = s.model()
    print("cost:", mod[cost].as_decimal(6))
    print("x:")
    for i in range(num_plants):
      for j in range(num_markets):
        print(mod[x[i][j]].as_decimal(6), end=" ")
      print()
    print()


  print("num_solutions:", num_solutions)


transp()
