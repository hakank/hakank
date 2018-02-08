#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Rabbit problem in Z3
#
# From http://www.cs.kuleuven.ac.be/~dtai/publications/files/21836.ps.gz
# "Constraint logic programming: applications and implementation", page 2 (footnote)
# """
# 9 animals, rabbits and pheasants are playing on the grass.
# We can see 24 legs. How many rabbits and pheasants are there?
# """
# 

#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

rabbits, pheasants = Ints('rabbits pheasants')

solve( rabbits + pheasants == 9,
       4*rabbits + 2 * pheasants == 24)
