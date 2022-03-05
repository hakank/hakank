#
# Two circle problem in z3.
#
# From Chris Smith Math Newsletter #516
# """
# Two circles.
#
# Add their circumferences and you'll get 10322*Pi.
# Add their areas and you'l get 13946281*Pi.
# Can you work out the radius of each circle?
# """
#
# Reminder:
#  - Area of a circle = Pi*r^2
#  - Circumference = 2*Pi*r

#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from math import pi
from z3 import *

def area(r):
    return pi*r*r

def circumference(r):
    return 2*pi*r

def two_circles():
    s = SolverFor("QF_LIRA")

    r1, r2 = Reals("r1 r2")

    s.add(r1 <= r2,
          circumference(r1) + circumference(r2) == 10322*pi,
          area(r1) + area(r2) == 13946281 * pi
          )

    while s.check() == sat:
        mod = s.model()
        print("r1:", mod[r1], "r2:", mod[r2])
        s.add(Or(r1 != mod[r1], r2 != mod[r2]))

print("pi:",pi)
two_circles()
