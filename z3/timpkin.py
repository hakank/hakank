#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Mrs Timpkin's Age problem in Z3
#
# From 
# http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/index.html
# """
# Mrs Timpkin's Age    from "Amusements in Mathematics, Dudeney", number 43.
#
# When the Timpkinses married eighteen years ago, Timpkins was three
# times as old as his wife, and today he is just twice as old as she.
# How old is Mrs. Timpkin? 
# """
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

T, W = Ints("T W")

solve(T - 18 == 3 * (W - 18), T == 2 * W)
