#  
# Geometric puzzle in z3.
#
# From https://stackoverflow.com/questions/63107267/solving-a-simple-geometric-puzzle-in-clpq-r-prolog
# """
# Consider the following square:
#
#     --------------------------    
#     | A                |     |
#     |                  |     |
#     --------------------     |
#     |      |           |     |
#     | B    |  C        | E   |
#     |      |           |     |
#     |      ------------|     |
#     |      |  D        |     |
#     |      |           |     |
#     |      |           |     |
#     -------------------------
#     
# 
# You are given three constraints:
#
#  - All rectangles (A, B, C, D and E) have the same area;
#  - Their geometric layout constitutes a square; and
#  - The height of A is 2.
#
# Now, I know this is very simple to solve by hand, but I thought it would be a very good
# example to show off the capabilities of CLP(Q/R) with Prolog:
#
# ...
# 
# """
#
# Solution:
#
# Exact:
# areas: [64/5, 64/5, 64/5, 64/5, 64/5]
# heights: [2, 6, 3, 3, 8]
# widths: [32/5, 32/15, 64/15, 64/15, 8/5]
# As float:
# areas: [12.8, 12.8, 12.8, 12.8, 12.8]
# heights: [2, 6, 3, 3, 8]
# widths: [6.4, 2.133333, 4.266666, 4.266666, 1.6]
#

# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 

from z3_utils_hakank import *

def geometric_puzzle():
    
    # s = SimpleSolver()
    s = SolverFor("QF_LIRA")
    # s = Optimize() # too slow
    
    a,b,c,d,e = Reals("a b c d e")
    areas = [a,b,c,d,e]
    
    aw,bw,cw,dw,ew = Reals("aw bw cw dw ew")
    ws = [aw,bw,cw,dw,ew]
    ah,bh,ch,dh,eh = Reals("ah bh ch dh eh")
    hs = [ah,bh,ch,dh,eh]

    s.add(a == b , b == c , c == d , d == e,
          a  >= 1, b  >= 1, c  >= 1, d  >= 1, e  >= 1,
          aw >= 1, bw >= 1, cw >= 1, dw >= 1, ew >= 1 ,
          ah == 2 ,
          a == ah * aw,
          b == bh * bw,
          c == ch * cw,
          d == dh * dw,
          e == eh * ew,
          
          bw + cw == aw,
          dw == cw ,
          ah + bh == eh,
          ch + dh == bh,
          aw + ew == eh
          )

    
    while s.check() == sat:
        mod = s.model()
        print("Exact:")
        print("areas:", [mod[t] for t in areas])
        print("heights:", [mod[t] for t in hs])
        print("widths:", [mod[t] for t in ws])
        print("As float:")
        print("areas:", [float(mod[t].as_decimal(6)) for t in areas])
        print("heights:", [mod[t] for t in hs])
        print("widths:", [float(mod[t].as_decimal(6).replace('?','')) for t in ws])                
        
        getDifferentSolution(s,mod,areas,hs,ws)


geometric_puzzle()
