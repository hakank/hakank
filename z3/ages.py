#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Ages problem in Z3
#
# Ages problem from Tony Hurlimann
# "Mathematical Modelling of Puzzle and Games", page 231
# """
# 1. Ten years from now Tim will be twice as old as Jane was when Mary
#    was nine times as old as Tim.

# 2. Eight years ago, Mary was half as old as Jane will be, when Jane is
#    one year older than Tim will be at the time, when Mary will be five
#    times as old as Tim will be two years from now.

# 3. When Tim was one year old, Mary was three years older than Tim
#    will be, when Jane is three times as old as Mary was six years before
#    the time, when Jane was half as old as Tim will be, when Mary will
#    be ten years older than Mary was, when Jane was one-third as old as
#    Tim will be, when Mary will be three times as old as she was, when
#    Jane was born.
#
# How old are the three persons now?
#
# 

# (Hurliman refers to http://mathforum.org/rec puzzles archive/logic/part1 )

# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

t,j,m,y,y1,y2,y3,y4,y5,y6,y7,y8 = Reals("t j m y y1 y2 y3 y4 y5 y6 y7 y8")
tim,jane,mary = Reals("time jane mary")


# constraints

sol.add(y + 10.0 - t == 2.0*(y1 - j)) 
sol.add(y1 - m == 9.0*(y1 - t)) 
sol.add(y - 8.0 - m == 1.0 / 2.0*(y2 - j)) 
sol.add(y2 - j == 1.0 + y3 - t) 
sol.add(y3 - m == 5.0*(y + 2.0 - t)) 
sol.add(t + 1.0 - m == 3.0 + y4 - t) 
sol.add(y4 - j == 3.0*(y5 - 6.0 - m)) 
sol.add(y5 - j == 1.0 / 2.0*(y6 - t)) 
sol.add(y6 - m == 10.0 + y7 - m) 
sol.add(y7 - j == 1.0 / 3.0*(y8 - t)) 
sol.add(y8 - m == 3.0*(j - m) )

sol.add(tim == y-t)
sol.add(jane == y-j)
sol.add(mary == y-m)

if sol.check() == sat:
    mod = sol.model()
    print("tim: ", mod[tim].as_decimal(1))
    print("jane: ", mod[jane].as_decimal(1))
    print("mary: ", mod[mary].as_decimal(1))
    print()

    

