#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Tommy's Birthday Coins coins puzzle in Z3
#
# From Martin Chlond Integer Programming Puzzles:
# http://www.chlond.demon.co.uk/puzzles/puzzles2.html, puzzle nr. 2.
# Description  : Tommy's Birthday Coins
# Source       : Clarke, L.H., (1954), Fun with Figures, William Heinemann Ltd.
# """
# 2. Tommy was given 15 coins for his birthday, all in half-crowns, shillings 
# and sixpences. When he added it up he found that he had 1 5s. 6d. 
# How many half-crowns was he given? (Clarke)
# """
# Answer: 8 half-crowns, 4 shillings and 3 sixpences
# This model was inspired by the XPress Mosel model created by Martin Chlond.
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
from z3_utils_hakank import *

sol = Solver()

coin = 3
value = [30,12,6]
x = IntVector("x",coin)

[sol.add(x[i] >= 1) for i in range(coin)]
sol.add(Sum([value[i]*x[i] for i in range(coin)]) == 306)
sol.add(Sum(x) == 15)

while sol.check() == sat:
    mod = sol.model()
    print [mod.eval(x[i]) for i in range(coin)]
    getDifferentSolution(sol,mod,x)
