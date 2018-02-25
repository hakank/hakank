#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Designing a box in Z3
#
# Design a box at minimum cost that meets
# area, volume, marketing and aesthetic requirements;
#
# AMPL:
# objective 50.96507524
# d  23.031
# w  9.5622
# h  6.86566
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = SolverFor("QF_NIA")
# sol = Solver()


d = Real("d")
w = Real("w")
h = Real("h")
z = Real("z") # to minimize

sol.add(z == 2.0*( 0.05*(d*w + d*h) + 0.1*w*h))

sol.add(z >= 0.0)
sol.add(d >= 0.0)
sol.add(w >= 0.0)
sol.add(h >= 0.0)
sol.add(2.0*(h*d + h*w + d*w) >= 888.0)
sol.add(h*d*w >= 1512.0)

# These two enforce aesthetics;
sol.add(h/w <= 0.718)
sol.add(h/w >= 0.518)

# Marketing requires a small footprint;
sol.add(d*w <= 252.0)

# sol.minimize(z)

while sol.check() == sat:
    mod = sol.model()
    print "z:", mod[z].as_decimal(4)
    print "d:", mod[d].as_decimal(4)
    print "w:", mod[w].as_decimal(4)
    print "h:", mod[h].as_decimal(4)
    print
    # getLessSolution(sol,mod,z)
    # Note: Here we have to manually check for the interval
    # otherwise z3 will give infinitely smaller differences
    sol.add(z < mod[z] - 0.001)

