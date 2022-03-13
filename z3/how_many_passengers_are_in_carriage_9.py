#
# How many passengers are in carriage 9 puzzle in Picat.

# MindYourDecisions:
# """
# A train has 11 passenger carriages. In any three consecutive passenger carriages, there are 
# exactly 99 passengers. If there are 381 passengers in total, how many passengers 
# are in carriage 9?
# """
#
# See the video https://www.youtube.com/watch?v=-rbozrj9poo
#
# Here we only checks for solutions that differs in carriage 9
# (x[8]) and we got only one solution (all is set to False):
#
#  x: [30, 54, 15, 30, 54, 15, 30, 54, 15, 30, 54]
#                                       
# Thus: carriage 9 has 15 passengers.
#
# As does carriages 3 and 6. All other carriages has 1..83
# possible number of passengers.
#
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
# 
from z3 import *


def carriage9(all=True):

    sol = SimpleSolver()

    n = 11

    x = [Int(f"x[{i}") for i in range(n)]
    for i in range(n):
        sol.add(x[i] >= 1, x[i] <= 99)

    sol.add(Sum(x) == 381)

    for i in range(n-2):
        sol.add(Sum([x[j] for j in range(i,i+2+1)]) == 99)

    num_solutions = 0
    while sol.check() == sat:
        num_solutions += 1
        mod = sol.model()
        print("x:", [mod.eval(x[i]) for i in range(n)])
        if all == True:
            sol.add(Or([x[i] != mod[x[i]] for i in range(n)]))
        else:
            # Just check if carriage 9 differs
            sol.add(x[8] != mod[x[8]])

    print("num_solutions:", num_solutions)


print("Only check carriage 9:")
carriage9(False)

print("\nAll solutions:")
carriage9(True)


