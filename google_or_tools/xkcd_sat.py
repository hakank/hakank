# Copyright 2021 Hakan Kjellerstrand hakank@gmail.com
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
"""

  xkcd problem (Knapsack) in OR-tools CP-SAT Solver.

  http://xkcd.com/287/

  Some amount (or none) of each dish should be ordered to give a total
  of exact 15.05

  This is a port of my old CP model xkcd.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from cp_sat_utils import ListPrinter

class SolutionPrinter(cp.CpSolverSolutionCallback):
    """SolutionPrinter"""
    def __init__(self, num_prices, products,price, z, x):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__num_prices = num_prices
        self.__products = products
        self.__price = price
        self.__z = z
        self.__x = x
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        print("z:", self.Value(self.__z) / 100.0)
        xval = [self.Value(self.__x[i]) for i in range(self.__num_prices)]
        print("x:", xval)
        for i in range(self.__num_prices):
          if xval[i] > 0:
            print(xval[i], "of", self.__products[i], ":", self.__price[i] / 100.0)
        print()


    def SolutionCount(self):
        return self.__solution_count


def main():

  model = cp.CpModel()

  #
  # data
  #
  num_prices = 6
  # for price and total: multiplied by 100 to be able to use integers
  price = [215, 275, 335, 355, 420, 580]
  total = 1505

  products = [
      "mixed fruit", "french fries", "side salad", "host wings",
      "mozzarella sticks", "samples place"
  ]

  # declare variables

  # how many items of each dish
  x = [model.NewIntVar(0, 10, "x%i" % i) for i in range(num_prices)]
  z = model.NewIntVar(0, 1505, "z")

  #
  # constraints
  #
  model.Add(z == sum([x[i] * price[i] for i in range(num_prices)]))
  model.Add(z == total)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  solution_printer = SolutionPrinter(num_prices, products,price, z, x)
  _status = solver.SearchForAllSolutions(model, solution_printer)

  print()
  print("NumSolutions:", solution_printer.SolutionCount())
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


if __name__ == "__main__":
  main()
