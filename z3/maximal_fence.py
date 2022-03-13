# 
# Maximal fence problems in z3.
#
# 1. A farmer has 140 ft fence available. 
#    What is the maximal possible area of a rectangle that can be created?
#
# 2. A farmer has 140 ft fence available. What is the maximal
#    possible area of a circle that can be created?
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
import math
from z3 import *

def maximal_fence1():
    
    # s = SimpleSolver()
    # s = Solver()
    s = SolverFor("QF_LIRA")
    # s = Optimize()

    f = 140

    r = f/(2*3.14159);
    r_area = r*r*3.14159;

    a = Real("a")
    s.add(a>=1, a <= f)
    
    b = Real("b")
    s.add(b>=1, b <= f, a <= b)
    
    y = Real("y")
    s.add(y == a*b, y >= 0)

    s.add(a*2 + 2*b == f)

    # s.maximize(y)

    while s.check() == sat:
         mod = s.model()
         print("a:", mod[a], "b:", mod[b], "y:", mod[y])
         s.add(y > mod[y])


#
# 2. A farmer has 140 ft fence available. What is the maximal
#    possible area of a circle that can be created?
#
# Note: This is not really an optimization problem.
# 
def maximal_fence2():
    
    s = SimpleSolver()
    # s = Solver()
    # s = Optimize()

    f = 140

    r = f/(2*math.pi)
    print("r:",r)

    # while s.check() == sat:
    #     mod = s.model()
    #     print("r:", mod[r])
    #     s.add(r > mod[r])

maximal_fence1()

# maximal_fence2()


