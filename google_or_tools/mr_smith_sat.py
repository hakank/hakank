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

  Mr Smith in OR-tools CP-SAT Solver.

  From an IF Prolog example (http://www.ifcomputer.de/)
  '''
  The Smith family and their three children want to pay a visit but they
  do not all have the time to do so. Following are few hints who will go
  and who will not:
      o If Mr Smith comes, his wife will come too.
      o At least one of their two sons Matt and John will come.
      o Either Mrs Smith or Tim will come, but not both.
      o Either Tim and John will come, or neither will come.
      o If Matt comes, then John and his father will
        also come.
  '''

  The answer should be:
   Mr_Smith_comes      =  0
   Mrs_Smith_comes     =  0
   Matt_comes          =  0
   John_comes          =  1
   Tim_comes           =  1

  This is a port of my old CP model mr_smith.py

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
  n = 5

  #
  # declare variables
  #
  x = [model.NewIntVar(0, 1, 'x[%i]' % i) for i in range(n)]
  Mr_Smith, Mrs_Smith, Matt, John, Tim = x

  #
  # constraints
  #

  #
  # I've kept the MiniZinc constraints for clarity
  # and debugging.
  #

  # If Mr Smith comes then his wife will come too.
  # (Mr_Smith -> Mrs_Smith)
  model.Add(Mr_Smith - Mrs_Smith <= 0)

  # At least one of their two sons Matt and John will come.
  # (Matt \/ John)
  model.Add(Matt + John >= 1)

  # Either Mrs Smith or Tim will come but not both.
  # bool2int(Mrs_Smith) + bool2int(Tim) = 1 /\
  # (Mrs_Smith xor Tim)
  model.Add(Mrs_Smith + Tim == 1)

  # Either Tim and John will come or neither will come.
  # (Tim = John)
  model.Add(Tim == John)

  # If Matt comes /\ then John and his father will also come.
  # (Matt -> (John /\ Mr_Smith))
  JohnAndMrSmith = model.NewBoolVar("JohnAndMrSmith")
  model.AddBoolAnd([John,Mr_Smith]).OnlyEnforceIf(JohnAndMrSmith)
  model.Add(Matt - (JohnAndMrSmith) <= 0)

  #
  # solution and search
  #
  solver = cp.CpSolver()
  status = solver.Solve(model)

  if status == cp.OPTIMAL:
    print('x:', [solver.Value(x[i]) for i in range(n)])

  print()
  print('NumConflicts:', solver.NumConflicts())
  print('NumBranches:', solver.NumBranches())
  print('WallTime:', solver.WallTime())


if __name__ == '__main__':
  main()
