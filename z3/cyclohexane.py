#
# Cyclohexane non-linear problem in z3
#


# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#
from z3 import *

def cyclohexane():
    s = Solver()

    x,y,z = Reals("x y z")
    # x,y,z = Ints("x y z")

    # s.add(x >= -20, x <= 20,
    #       y >= -20, y <= 20,
    #       z >= -20, y <= 20)

    s.add(13 + y*y*(1+z*z) + z*(z - 24*y) == 0,
          13 + z*z*(1+x*x) + x*(x - 24*z) == 0,
          13 + x*x*(1+y*y) + y*(y - 24*x) == 0),

    if s.check() == sat:
        mod = s.model()
        print(mod)
        # s.add(Or([x != mod[x], y != mod[y], z != mod[z]]))
        

cyclohexane()
