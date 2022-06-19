"""
Secret Santa problem in cpmpy.

From Ruby Quiz Secret Santa
http://www.rubyquiz.com/quiz2.html
'''
Honoring a long standing tradition started by my wife's dad, my friends
all play a Secret Santa game around Christmas time. We draw names and
spend a week sneaking that person gifts and clues to our identity. On the
last night of the game, we get together, have dinner, share stories, and,
most importantly, try to guess who our Secret Santa was. It's a crazily
fun way to enjoy each other's company during the holidays.

To choose Santas, we use to draw names out of a hat. This system was
tedious, prone to many 'Wait, I got myself...' problems. This year, we
made a change to the rules that further complicated picking and we knew
the hat draw would not stand up to the challenge. Naturally, to solve
this problem, I scripted the process. Since that turned out to be more
interesting than I had expected, I decided to share.

This weeks Ruby Quiz is to implement a Secret Santa selection script.

Your script will be fed a list of names on STDIN.
...
Your script should then choose a Secret Santa for every name in the list.
Obviously, a person cannot be their own Secret Santa. In addition, my friends
no longer allow people in the same family to be Santas for each other and your
script should take this into account.
'''

Comment: This model skips the file input and mail parts. We
    assume that the friends are identified with a number from 1..n,
    and the families is identified with a number 1..num_families.

There is a huge number of solutions, namely 4089600.

Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *



def secret_santa():

  model = Model()

  # data
  family = [1, 1, 1, 1, 2, 3, 3, 3, 3, 3, 4, 4]
  num_families = max(family)
  n = len(family)

  # declare variables
  x = intvar(0,n-1,shape=n,name="x")

  # constraints
  model += [AllDifferent(x)]

  # Can't be one own's Secret Santa
  # Ensure that there are no fix-point in the array
  for i in range(n):
    model += [x[i] != i]

  # No Secret Santa to a person in the same family
  for i in range(n):
    model += [family[i] != Element(family, x[i])]
    # model += [family[i] != family[x[i]]] # Don't work

  num_solutions = model.solveAll(display=x)
  print("num_solutions:",num_solutions)

secret_santa()
