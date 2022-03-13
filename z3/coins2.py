# 
# Coin problem in z3.
#
# From http://www.chlond.demon.co.uk/puzzles/puzzles1.html (not working anymore)
# """
# 2. Supposing that eleven coins with round holes are worth 15 bits, while eleven square
#    ones are worth 16 bits, and eleven of triangular shape are worth 17 bits, tell how
#    many round, square or triangular pieces of cash would be required to purchase an item
#    worth eleven bits. (Loyd) 
# """
# 
# Answer: 7 coins with round holes, 1 coin with a square hole.
# 
#  obj: 8
#  x:15/11 (1.363636?) y:16/11 (1.454545? z:17/11 (1.545454?)
#  a: 7 b: 1 c: 0
#

#
# This z3 model was created by Hakan Kjellerstrand, hakank@gmail.com
# See also my z3 page: http://www.hakank.org/z3/
#

from z3 import *

def coins2():
    s = Solver() # For("LRA") # For("QF_LIRA")
    
    a,b,c = Ints("a b c")
    x,y,z = Reals("x y z")
    obj = Int("z")

    s.add(x >= 0.0, y >= 0.0, z >= 0.0,
          a >= 0, b >= 0, c >= 0, 
          11.0 * x == 15.0,
          11.0 * y == 16.0,
          11.0 * z == 17.0,
          a*x + b*y + c*z == 11,
          obj == a+b+c
        )
    
    while s.check() == sat:
        mod = s.model()
        print("obj:", mod[obj])
        print(f"x:{mod[x]} ({mod[x].as_decimal(6)}) y:{mod[y]} ({mod[y].as_decimal(6)} z:{mod[z]} ({mod[z].as_decimal(6)})")        
        print("a:", mod[a], "b:", mod[b], "c:", mod[c])        
        print()
        # s.add(Or(x != mod[x], y != mod[y], z != mod[z],
        #          a != mod[a], b != mod[b], c != mod[c],
        #          ))
        s.add(obj < mod[obj])
  

coins2()
