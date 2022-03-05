#
# Working together problem in z3.
# 
# From https://mindyourdecisions.com/blog/2017/08/13/can-you-solve-a-math-word-problem-that-stumps-us-college-students-a-working-together-problem/
# """
# Alice and Bob can complete a job in 2 hours.
# Alice and Charlie can complete the same job in 3 hours.
# Bob and Charlie can complete the same job in 4 hours.
# How long will the job take if Alice, Bob, and Charlie work together?
#
# Assume each person works at a constant rate, whether working alone 
# or working with others.
# """
#
# [total = 24/13, alice = 7/24, bob = 5/24, charlie = 1/24]
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3 import *

def working_together():
    s = SimpleSolver()

    alice, bob, charlie, total = Reals("alice bob charlie total")

    s.add(2*alice + 2*bob     == 1,
          3*alice + 3*charlie == 1,
          4*bob   + 4*charlie == 1,
          total*(alice+bob+charlie) == 1)

    while s.check() == sat:
        mod = s.model()
        print(mod)
        s.add(Or(alice != mod[alice], bob != mod[bob],
                 charlie != mod[charlie], total != mod[total]))

working_together()

