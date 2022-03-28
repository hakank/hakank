#
# Final Student puzzle in z3.
#
#  From Chris Smith's MathNewsletter #555
# """
# Mrs Krabappel gave her class a test
# and she’s almost marked them all, just
# one to go
#
# If the final student scores just 7% then
# the class average will be 90% but,
# even more remarkably, if they
# score 92% then they’ll be looking at a
# class average of 95% (and Mrs K will
# be in line for a bonus).
# How many students are in the class?
# """

# Solutions:
# Final student1:
# n: 17
# x: ['61.5', '61.5', '100', '100', '100', '100', '100', '100', '100', '100', '100', '100', '100', '100', '100', '100']
#
# Final student2:
# n: 17
# x: [0, 6, 17, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100]
#
# Final student3:
# n: 17 y: 1523
#

# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#

from z3 import *


def final_student1(n):

    s = SimpleSolver()

    x = [Real(f"x[{i}") for i in range(n-1)]
    for i in range(n-1):
        s.add(x[i] >= 0, x[i] <= 100)

    # Symmetry
    for i in range(1,n-1):
        s.add(x[i-1] <= x[i])
    
    s.add(90*n ==  7+Sum([x[i] for i in range(n-1)]))
    s.add(95*n == 92+Sum([x[i] for i in range(n-1)]))

    if s.check() == sat:
        print("n:",n)
        mod = s.model()
        print("x:", [mod[x[i]].as_decimal(6) for i in range(n-1)])
        s.add(Or([x[i] != mod[x[i]] for i in range(n-1)]))

#
# Another approach: Let n be a decision variable for us to find.
# Here we use Ints instead (but Reals works fine as well).
# 
# It requires that we have some max value
# for the loops. 
# 
def final_student2():
    
    s = SimpleSolver()

    max_n = 120 # max value of n

    n = Int("n")

    x = [Int(f"x[{i}") for i in range(max_n)]
    # x = [Real(f"x[{i}") for i in range(max_n)]    
    for i in range(max_n):
        s.add(x[i] >= 0, x[i] <= 100)

    # Symmetry
    for i in range(1,max_n):
        s.add(x[i-1] <= x[i])
    
    s.add(90*n ==  7+Sum([If(i <= n, x[i],0) for i in range(max_n)]))
    s.add(95*n == 92+Sum([If(i <= n, x[i],0) for i in range(max_n)]))

    if s.check() == sat:
        mod = s.model()
        n_val = mod[n].as_long()
        print("n:", n_val)
        print("x:", [mod[x[i]] for i in range(n_val)])
        s.add(Or([x[i] != mod[x[i]] for i in range(n_val)]))

#
# And here's an algebraic take on this.
#
def final_student3():
    s = SimpleSolver()
    
    n, y = Ints("n y")

    s.add(90*n == 7+y, 95*n == 92+y)

    while s.check() == sat:
        mod = s.model()
        print("n:",mod[n], "y:", mod[y])
        s.add(Or(n != mod[n], y != mod[y]))

    

print("Final student1:")
for n in range(1,30):
    final_student1(n)


print("\nFinal student2:")
final_student2()

print("\nFinal student3:")
final_student3()
