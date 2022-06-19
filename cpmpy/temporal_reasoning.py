"""
Temporal reasoning in cpmpy.


From Krzysztof R. Apt 'Principle of Constraint Programming', page 23ff
'''
Consider the following problem:

The meeting ran non-stop the whole day.
Each person stayed at the meeting for a continous period of time.
The meeting began while Mr Jones was present and finished
while Ms White was present.
Ms_White arrived after the meeting has began.
In turn, Director Smith, was also present but he arrived after
Jones had left.
Mr Brown talked to Ms White in presence of Smith.
Could possibly Jones and White have talked during this meeting?
'''

Also see the presentation 
http://homepages.cwi.nl/~apt/pcp/ch2-sli.pdf.gz, page 15ff)

The solution in Apt's presentation is: [0,3,1,5,0,5,4,5,2,6]


In total there are 32010 solutions (times are in 0..9).
Here is one optimal solution (minimize Z, the sum of the end times
of Jones and White):

  [0, 2, 1, 4, 0, 9, 3, 9, 2, 5]
  Jones  : 0..2
  Brown  : 0..9
  Smith  : 3..9
  White  : 2..5
  Meeting: 1..4
  Z: 7


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

#
# Here are the temporal constraints
# that are defined in the book by Apt (op.cit).
#
def t_interval(X1, X2):
  return [X1 < X2]


def before(X1, X2, Y1, Y2):
  return [t_interval(X1, X2),
          t_interval(Y1, Y2),
          X2 < Y1]


def after(X1, X2, Y1, Y2):
  return before(Y1, Y2, X1, X2)


def meets(X1, X2, Y1, Y2):
  return [t_interval(X1, X2),
          t_interval(Y1, Y2),
          X2 == Y1]

def met_by(X1, X2, Y1, Y2):
  return [meets(Y1, Y2, X1, X2)]

def overlaps(X1, X2, Y1, Y2):
  return [t_interval(X1, X2),
          t_interval(Y1, Y2),
          X1 < Y1,
          Y1 < X2,
          X2 < Y2]

def overlapped_by(X1, X2, Y1, Y2):
  return [overlaps(Y1, Y2, X1, X2)]

def starts(X1, X2, Y1, Y2):
  return [t_interval(X1, X2),
          t_interval(Y1, Y2),
          X1 == Y1,
          X2 < Y2]

def started_by(X1, X2, Y1, Y2):
  return [starts(Y1, Y2, X1, X2)]


def during(X1, X2, Y1, Y2):
  return [t_interval(X1, X2),
          t_interval(Y1, Y2),
          X1 > Y1,
          X2 < Y2]

def t_contains(X1, X2, Y1, Y2):
  return [during(Y1, Y2, X1, X2)]


def finishes(X1, X2, Y1, Y2):
  return [t_interval(X1, X2),
          t_interval(Y1, Y2),
          X1 > Y1,
          X2 == Y2]

def finished_by(X1, X2, Y1, Y2):
  return [finishes(Y1, Y2, X1, X2)]


def t_equal(X1, X2, Y1, Y2):
  return [t_interval(X1, X2),
          t_interval(Y1, Y2),
          X1 == Y1,
          X2 == Y2]

def real_overlap(X1, X2, Y1, Y2):
  return [X1 < Y2,
          Y1 < X2]


def weak_overlap(X1, X2, Y1, Y2):
  return [X1 <= Y2,
          Y1 <= X2]

def temporal_reasoning(opt=True):
  
  # Coding:
  #
  # J: Jones
  # B: Brown
  # S: Smith
  # W: White
  # M: Meeting

  max_time = 9

  # variables
  J1 = intvar(0,max_time,name="J1")
  J2 = intvar(0,max_time,name="J2")
  M1 = intvar(0,max_time,name="M1")
  M2 = intvar(0,max_time,name="M2")
  B1 = intvar(0,max_time,name="B1")
  B2 = intvar(0,max_time,name="B2")
  S1 = intvar(0,max_time,name="S1")
  S2 = intvar(0,max_time,name="S2")
  W1 = intvar(0,max_time,name="W1")
  W2 = intvar(0,max_time,name="W2")

  Z = intvar(0,max_time*2,name="Z")

  All = [J1,J2,M1,M2,B1,B2,S1,S2,W1,W2]

  #
  # The story
  # 
  model = Model([
                  # Meeting and Jones
                  J1 < M1,
                  M1 < J2,
                  
                  # Meeting and White  
                  overlaps(M1, M2, W1, W2),
                  
                  # Meeting and Smith
                  real_overlap(M1, M2, S1, S2),
    
                  # Jones and Smith
                  before(J1, J2, S1, S2),
    
                  # Brown and Smith
                  real_overlap(B1, B2, S1, S2),

                  # Brown and White
                  real_overlap(B1, B2, W1, W2),

                  # Smith and White
                  real_overlap(S1, S2, W1, W2),

                  # "Could possibly Jones and White have talked
                  # during this meeting?"
                  weak_overlap(J1, J2, W1, W2),

    Z == J2 + W2

    ]
    )

  if opt:
    model.minimize(Z)

  def print_sol():
    print([a.value() for a in All])
    print(f"Jones  : {J1.value()}..{J2.value()}")
    print(f"Brown  : {B1.value()}..{B2.value()}")
    print(f"Smith  : {S1.value()}..{S2.value()}")
    print(f"White  : {W1.value()}..{W2.value()}")
    print(f"Meeting: {M1.value()}..{M2.value()}")
    print("Z:",Z.value())
    print()

    
  ss = CPM_ortools(model)
  # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
  # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
  # ss.ort_solver.parameters.cp_model_presolve = False
  ss.ort_solver.parameters.linearization_level = 0
  ss.ort_solver.parameters.cp_model_probing_level = 0

  num_solutions = ss.solveAll(display=print_sol)
  print("num_solutions:", num_solutions  )


print("Generate all solutions:")
opt = False
temporal_reasoning(opt)

print("Find an optimal solution (minimize the endtimes of Jones and White):")
opt = True
opt = temporal_reasoning(opt)
