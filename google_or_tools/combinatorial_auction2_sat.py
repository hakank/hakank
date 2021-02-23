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
  Combinatorial auction in OR-tools CP-SAT Solver.

  This is a more general model for the combinatorial example
  in the Numberjack Tutorial, pages 9 and 24 (slides  19/175 and
  51/175).

  The original and more talkative model is here:
  http://www.hakank.org/numberjack/combinatorial_auction.py

  This is port of my old OR-tools CP model combinatorial_auction2.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
from collections import defaultdict
from cp_sat_utils import scalar_product


def main():

  model = cp.CpModel()

  N = 5

  # the items for each bid
  items = [
      [0, 1],     # A,B
      [0, 2],     # A, C
      [1, 3],     # B,D
      [1, 2, 3],  # B,C,D
      [0]         # A
  ]
  # collect the bids for each item
  items_t = defaultdict(list)

  # [items_t.setdefault(j,[]).append(i) for i in range(N) for j in items[i] ]
  # nicer:
  [items_t[j].append(i) for i in range(N) for j in items[i]]

  bid_amount = [10, 20, 30, 40, 14]

  #
  # declare variables
  #
  X = [model.NewBoolVar("x%i" % i) for i in range(N)]
  obj = model.NewIntVar(0, 100, "obj")

  #
  # constraints
  #
  # model.Add(obj == solver.ScalProd(X, bid_amount))
  scalar_product(model, X, bid_amount,obj )

  for item in items_t:
    model.Add(sum([X[bid] for bid in items_t[item]]) <= 1)

  # objective
  model.Maximize(obj)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  status = solver.Solve(model)

  if status == cp.OPTIMAL:
    print("X:", [solver.Value(X[i]) for i in range(N)])
    print("obj:", solver.Value(obj))

  print()
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


if __name__ == "__main__":
  main()
