#!/usr/bin/env python3

# Problem from https://stackoverflow.com/questions/66079294/what-is-the-most-elegant-way-to-find-16-bit-numbers-which-satisfy-some-condition

from z3 import *

# Answers should be:
# [x = 16673,y = 18603]
# z = [4871,4999,5895,6023]

solver = Solver()
x = BitVec('x', 16)
y = BitVec('y', 16)
z = BitVec('z', 16)

solver.add(y | x == 0x49ab)
solver.add((y >> 2) ^ x == 0x530b)
solver.add((z >> 1) & y == 0x0883)
solver.add((x << 2) | z == 0x1787)

num_solutions = 0
while solver.check() == sat:
    num_solutions += 1
    m = solver.model()

    xval = m.eval(x)
    yval = m.eval(y)
    zval = m.eval(z)
    print([xval,yval,zval])
    # Ensure that we don't get this solution again
    solver.add(Or([x!=xval,y!=yval,z!=zval]))
    
print("num_solutions:", num_solutions)
