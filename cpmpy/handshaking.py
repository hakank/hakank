"""
Halmos' handshake problem in cpmpy.

Problem formulation from Alloy (examples/puzzles/handshake)
'''
Alloy model of the Halmos handshake problem

Hilary and Jocelyn are married. They invite four couples who are friends for dinner. When
they arrive, they shake hands with each other. Nobody shakes hands with him or herself
or with his or her spouse. After there has been some handshaking, Jocelyn jumps up on
a chair and says 'Stop shaking hands!', and then asks how many hands each person has
shaken. All the answers are different. How many hands has Hilary shaken?

The Alloy model represents the problem as a set of constraints. Properties of the spouse
relationship and of handshaking in general are given as facts. The particular situation
is cast as a function.

There are 9 people answering, and all answers are different. Nobody can shake more than
8 hands. So answers must be 0..8. The one (p8 say) who answered 8 has shaken everybody's
hand except for his or her own, and his or her spouse's. Now consider the person who shook
0 hands (p0 say). The persons p0 and p8 are distinct. If they are not married, then p8 cannot
have shaken 8 hands, because he or she did not shake the hand of p0 or of his or her spouse.
So p8's spouse to p0. Now imagine Jocelyn asking the question again, with p0 and p8 out of
the room, and excluding hand shakes with them. Since p8 shook hands with everyone else
except p0 and p8, everyone gives an answer one smaller than they did before, giving 0..6.
The argument now applies recursively. So Hilary is left alone, having shaken 4 hands. 
'''
Alloy is here: http://alloy.mit.edu/alloy

Also, see the following that discuss Halmos' Handshake problem
http://docs.law.gwu.edu/facweb/jsiegel/Personal/math/mathhome.htm#halmos
    http://docs.law.gwu.edu/facweb/jsiegel/Personal/math/shakeanswer.htm

The origin of the problem seems to be
P.R. Halmos: "To Count or to Think, That is the Question", page 1ff
http://bernoulli.math.rug.nl/vorigelezingen/lezing03/lezing03.pdf


Symmetry breaking, the order of the couples besides the hosts.

For n = 10:
Without it: 384 solutions (all x = [4,4,.....])
With it: 1 solution: x: [4, 4, 0, 8, 1, 7, 2, 6, 3, 5] 
          (since we order 0,1,2,3 shakes)
 
Note that all number of handshaking of the pairs sums to 8, 
i.e. 4+4, 0+8, 1+7, 2+6, 3+5
More general: The number of handshaking per pair sums to n-2.

This 'formula' is exploited if we are using symmetry breaking.
See calculate_x() for details.


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys,math
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *
import time


#
# The formula for calulating the distinct solution of X
# (the number of shaken hands) for a certain N and with symmetry breaking:
#
#   N2 #= N-2
#   [N div 2, N div 2, 0, N2-2, 1, N2-3, 2, N2-4, 3, ...,  1+ (N div 2)]
#
#                1  2  3  4  5  6  7  8  9 10
# For N=10, X = [4, 4, 0, 8, 1, 7, 2, 6, 3, 5] 
#
def calculate_x(n, x):
  ndiv2 = math.ceil(n/2)
  n2 = n-2
  constraints = [x[0] == ndiv2-1,
                 x[1] == ndiv2-1,
                 x[2] == 0,
                 x[3] == n2
                 ]
  for i in range(4,n):
    idiv2 = math.ceil(i/2)
    if i % 2 == 0:
      constraints += [x[i] == idiv2-1]
    else:
      constraints += [x[i] == n-idiv2]

  return constraints

def handshaking(n=10,symmetry=False,num_sols=0,print_solutions=True):

  # decision variables

  # can shake max n-2 hands
  #   coded Pair1a,Pair1b,  Pair2a,Pair2b, ...
  x = intvar(0,n-2,shape=n,name="x")


  # who shake with whom:
  #  (not him/herself and not his/her spouse)
  y = boolvar(shape=(n,n),name="y")
  y_flat = [y[i,j] for i in range(n) for j in range(n)]

  model = Model()

  #
  # We assume that Hilary is in position x[0]
  # (and Hilary's spouse - Jocelyn - in x[1])
  # All except Hilary's counts are different.
  #
  model += [AllDifferent(x[1:])]

   
  for i in range(math.ceil(n/2)):
    # don't shake hand with spouse
    model += [y[2*i,2*i+1] == 0,
              y[2*i+1,2*i] == 0 ]

  for i in range(n):
    model += [ y[i,i] == 0,       # don't shake hand with oneself        
               x[i] == y[i].sum() # how many hands has x[i] shaken
              ]

  for i in range(n):
    for j in range(n):
      # symmetry of handshaking:
      #    a shake hands with b <-> b shake hands with a
      model += [y[i,j] == y[j,i]]


  if symmetry == True:
    ndiv2 = math.ceil(n/2)
    model += [calculate_x(n,x),
              # These constraints are not needed
              # increasing([x[2+2*i] for i in range(ndiv2-1)]),
              # decreasing([x[3+2*i] for i in range(ndiv2-1)]),              
              # [x[2+2*i] < x[2+2*i+1] for i in range(ndiv2-2)]
              ]

  def print_sol():
    if print_solutions:
      print("x:")
      print(x.value())
      n = len(x)
      print("y:")
      for i in range(n):
        for j in range(n):
          print(y[(i,j)].value(),end=" ")
        print()
      print()
      print()

  ss = CPM_ortools(model)
  ss.ort_solver.parameters.cp_model_probing_level = 0
  ss.ort_solver.parameters.linearization_level =  0
  
  num_solutions = ss.solveAll(solution_limit=num_sols,display=print_sol)
  print("num_solutions:",num_solutions)


n = 10
handshaking(n)
print("\nWith symmetry breaking:")
handshaking(n,True,0,True)
print("\nn=20. Print first non symmetric solution:")
handshaking(20,False,1,True)

print("\nn=100. Print first non symmetric solution:")
handshaking(100,False,1,True)
