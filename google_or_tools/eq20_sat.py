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

  Eq 20 in OR-tools CP-SAT Solver.

  Standard benchmark problem.

  This is a port of my old CP model eq20.py

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/
"""
from __future__ import print_function
from ortools.sat.python import cp_model as cp
import math, sys
# from cp_sat_utils import *


def main():

  model = cp.CpModel()

  #
  # data
  #
  n = 7

  #
  # variables
  #
  X = [model.NewIntVar(0, 10, "X(%i)" % i) for i in range(n)]
  X0, X1, X2, X3, X4, X5, X6 = X

  #
  # constraints
  #
  model.Add(-76706 * X0 + 98205 * X1 + 23445 * X2 + 67921 * X3 + 24111 * X4 +
             -48614 * X5 + -41906 * X6 == 821228)
  model.Add(87059 * X0 + -29101 * X1 + -5513 * X2 + -21219 * X3 + 22128 * X4 +
             7276 * X5 + 57308 * X6 == 22167)
  model.Add(-60113 * X0 + 29475 * X1 + 34421 * X2 + -76870 * X3 + 62646 * X4 +
             29278 * X5 + -15212 * X6 == 251591)
  model.Add(49149 * X0 + 52871 * X1 + -7132 * X2 + 56728 * X3 + -33576 * X4 +
             -49530 * X5 + -62089 * X6 == 146074)
  model.Add(-10343 * X0 + 87758 * X1 + -11782 * X2 + 19346 * X3 + 70072 * X4 +
             -36991 * X5 + 44529 * X6 == 740061)
  model.Add(85176 * X0 + -95332 * X1 + -1268 * X2 + 57898 * X3 + 15883 * X4 +
             50547 * X5 + 83287 * X6 == 373854)
  model.Add(-85698 * X0 + 29958 * X1 + 57308 * X2 + 48789 * X3 + -78219 * X4 +
             4657 * X5 + 34539 * X6 == 249912)
  model.Add(-67456 * X0 + 84750 * X1 + -51553 * X2 + 21239 * X3 + 81675 * X4 +
             -99395 * X5 + -4254 * X6 == 277271)
  model.Add(94016 * X0 + -82071 * X1 + 35961 * X2 + 66597 * X3 + -30705 * X4 +
             -44404 * X5 + -38304 * X6 == 25334)
  model.Add(-60301 * X0 + 31227 * X1 + 93951 * X2 + 73889 * X3 + 81526 * X4 +
             -72702 * X5 + 68026 * X6 == 1410723)
  model.Add(-16835 * X0 + 47385 * X1 + 97715 * X2 + -12640 * X3 + 69028 * X4 +
             76212 * X5 + -81102 * X6 == 1244857)
  model.Add(-43277 * X0 + 43525 * X1 + 92298 * X2 + 58630 * X3 + 92590 * X4 +
             -9372 * X5 + -60227 * X6 == 1503588)
  model.Add(-64919 * X0 + 80460 * X1 + 90840 * X2 + -59624 * X3 + -75542 * X4 +
             25145 * X5 + -47935 * X6 == 18465)
  model.Add(-45086 * X0 + 51830 * X1 + -4578 * X2 + 96120 * X3 + 21231 * X4 +
             97919 * X5 + 65651 * X6 == 1198280)
  model.Add(85268 * X0 + 54180 * X1 + -18810 * X2 + -48219 * X3 + 6013 * X4 +
             78169 * X5 + -79785 * X6 == 90614)
  model.Add(8874 * X0 + -58412 * X1 + 73947 * X2 + 17147 * X3 + 62335 * X4 +
             16005 * X5 + 8632 * X6 == 752447)
  model.Add(71202 * X0 + -11119 * X1 + 73017 * X2 + -38875 * X3 + -14413 * X4 +
             -29234 * X5 + 72370 * X6 == 129768)
  model.Add(1671 * X0 + -34121 * X1 + 10763 * X2 + 80609 * X3 + 42532 * X4 +
             93520 * X5 + -33488 * X6 == 915683)
  model.Add(51637 * X0 + 67761 * X1 + 95951 * X2 + 3834 * X3 + -96722 * X4 +
             59190 * X5 + 15280 * X6 == 533909)
  model.Add(-16105 * X0 + 62397 * X1 + -6704 * X2 + 43340 * X3 + 95100 * X4 +
             -68610 * X5 + 58301 * X6 == 876370)

  #
  # search and result
  #
  solver = cp.CpSolver()
  status = solver.Solve(model)

  if status == cp.OPTIMAL:
    print("X:", [solver.Value(X[i]) for i in range(n)])
    print()

  print()
  print("NumConflicts:", solver.NumConflicts())
  print("NumBranches:", solver.NumBranches())
  print("WallTime:", solver.WallTime())


if __name__ == "__main__":
  main()
