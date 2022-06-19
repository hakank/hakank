"""
Monks and doors problem in cpmpy.

From http://user.it.uu.se/~rolandb/LP/gammal/960615_facit.ps
'''
There is a room with four doors and eight monks. One or more of
the doors may be exit. Each monk is either telling a lie or the truth.

The monks make the following statements:
Monk 1: Door A is the exit.
Monk 2: At least one of the doors B and C is the exit.
Monk 3: Monk 1 and Monk 2 are telling the truth.
Monk 4: Doors A and B are both exits.
Monk 5: Doors A and B are both exits.
Monk 6: Either Monk 4 or Monk 5 is telling the truth.
Monk 7: If Monk 3 is telling the truth, so is Monk 6.
Monk 8: If Monk 7 and Monk 8 are telling the truth, so is Monk 1.

Which door is an exit no matter who is a liar and who is telling the
truth.
'''

Answer: Door A is an exit.
        And monks 1, 7, and 8 are telling the truth.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *


def monks_and_doors():
  
  # variables
  doors = boolvar(shape=4)
  A,B,C,D = doors
  A.name="A";B.name="B";C.name="C";D.name="D"
  
  monks = boolvar(shape=8)
  M1,M2,M3,M4,M5,M6,M7,M8 = monks
  M1.name="M1";M2.name="M2";M3.name="M3";M4.name="M4";
  M5.name="M5";M6.name="M6";M7.name="M7";M8.name="M8";

  # constraints
  model = Model([
    # Monk 1: Door A is the exit.
    M1 == A,
  
    # Monk 2: At least one of the doors B and C is the exit.
    M2 == ( B | C),
  
    # Monk 3: Monk 1 and Monk 2 are telling the truth.
    M3 == (M1 & M2),
  
    # Monk 4: Doors A and B are both exits.
    M4 == (A & B),

    # Monk 5: Doors A and C are both exits.
    M5 == (A & C),

    # Monk 6: Either Monk 4 or Monk 5 is telling the truth.
    M6 == (M4 ^ M5), 
    # M6 == (M4 + M5 == 1), % alternative encoding

    # Monk 7: If Monk 3 is telling the truth, so is Monk 6.
    M7 == (M3.implies(M6)),

    # Monk 8: If Monk 7 and Monk 8 are telling the truth, so is Monk 1.
    M8 == ((M7 & M8).implies(M1)),

    # Exactly one door is an exit.
    sum(doors) == 1
    ])

  print(model)

  def print_sol():
    print("monks:", monks.value(), "exits doors:",[d for d in doors if d.value() == 1] )
    print("doors:", doors.value(), "telling truth:", [m for m in monks if m.value() == 1])
    print()

  
  ss = CPM_ortools(model)
  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions)


monks_and_doors()
