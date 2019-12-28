#!/usr/bin/python -u
# -*- coding: latin-1 -*-
# 
# Fibonacci (bidirectional) in Z3
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
# 
from z3 import *

# From https://rise4fun.com/Z3/0pld:
# """
# (declare-fun fib (Int) Int)
# (assert (= 1 (fib 0)))
# (assert (= 1 (fib 1)))
# (assert (forall (x Int) (=> (>= x 2) (= (fib x) (+ (fib (- x 1)) (fib (- x 2)))))))
# (assert (= 2 (fib 2)))
# """

sol = Solver()

max_n = 31

#
# Note: One have to set a max limit on fib
#
# https://stackoverflow.com/questions/6915227/can-z3-check-the-satisfiability-of-formulas-that-contain-recursive-functions
# Leonardo de Moura:
# """
# The models produced by Z3 assign an interpretation for each uninterpreted function symbol. The models can
# be viewed as functional programs. The current version does not produce recursive definitions.
# The first example [Fibonacci] is satisfiable, but Z3 fails to produce an interpretation for fib because
# it does not support recursive definitions. We have plans to extend Z3 in this direction.
# """
fib = Function("fib", IntSort(), IntSort())
x = Int("x")
# sol.add(fib(0) == 1)
# sol.add(fib(1) == 1)
# sol.add(ForAll(x, Implies(And(x >= 2, x <= max_n), fib(x) == fib(x-1) + fib(x-2))))
# Simpler:
sol.add(ForAll(x, If(And(x >= 2, x <= max_n), fib(x) == fib(x-1) + fib(x-2), fib(x) == 1)))

# sol.add(x == fib(2))
y = Int("y")
z = Int("z")
sol.add(y>0, y <= max_n, z >0, z <= max_n)

sol.add(10946 == fib(y))
sol.add(2178309 == fib(z))

print(sol)
if sol.check()==sat:
    mod = sol.model()
    # print("x:", mod.eval(x))
    print("z:", mod.eval(z), "y:", mod.eval(y))
    sol.add(z != mod.eval(z),y != mod.eval(y))
