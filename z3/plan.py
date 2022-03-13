# 
# Planning problem in z3.
#
# From GLPK:s example plan.mod
# 
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
# 
import math
from z3 import *

def plan():
    
    # s = SimpleSolver()
    # s = Solver()
    # s = SolverFor("QF_LIRA")
    s = Optimize()

    bin1,bin2,bin3,bin4,bin5,alum,silicon = Reals("bin1 bin2 bin3 bin4 bin5 alum silicon")
    params = [bin1,bin2,bin3,bin4,bin5,alum,silicon]
    s.add(bin1>= 0.0, bin1<=200.0)
    s.add(bin2>= 0.0, bin2<=2500.0)
    s.add(bin3>= 400.0, bin3<=800.0)
    s.add(bin4>= 100.0, bin4<=700.0)
    s.add(bin5>= 0.0, bin5<=1500.0)
    s.add(alum>= 0.0, alum<=1000.0)
    s.add(silicon>= 0.0, silicon<=1000.0)

    value = Real("value")
    s.add(value == 0.03 * bin1 + 0.08 * bin2 + 0.17 * bin3 + 0.12 * bin4 + 0.15 * bin5 + 0.21 * alum + 0.38 * silicon)

    s.add(bin1 + bin2 + bin3 + bin4 + bin5 + alum + silicon == 2000.0,
          0.15 * bin1 + 0.04 * bin2 + 0.02 * bin3 + 0.04 * bin4 + 0.02 * bin5 +
          0.01 * alum + 0.03 * silicon <= 60.0,
          0.03 * bin1 + 0.05 * bin2 + 0.08 * bin3 + 0.02 * bin4 + 0.06 * bin5 +
          0.01 * alum <= 100.0,
          0.02 * bin1 + 0.04 * bin2 + 0.01 * bin3 + 0.02 * bin4 + 0.02 * bin5 <= 40.0,
          0.02 * bin1 + 0.03 * bin2 + 0.01 * bin5 <= 30.0,
          0.70 * bin1 + 0.75 * bin2 + 0.80 * bin3 + 0.75 * bin4 + 0.80 * bin5 + 0.97 * alum >= 1500.0,
          250.0 <= 0.02 * bin1 + 0.06 * bin2 + 0.08 * bin3 + 0.12 * bin4 + 0.02 * bin5 +     0.01 * alum + 0.97 * silicon,
          0.02 * bin1 + 0.06 * bin2 + 0.08 * bin3 + 0.12 * bin4 + 0.02 * bin5 + 0.01 * alum + 0.97 * silicon <= 300.0
        )

    s.minimize(value)

    if s.check() == sat:
         mod = s.model()
         print("value:", mod[value].as_decimal(6))
         print("bin1:", mod[bin1].as_decimal(6), "bin2:", mod[bin2].as_decimal(6), "bin3:", mod[bin3].as_decimal(6))
         print("bin4:", mod[bin4].as_decimal(6), "bin5:", mod[bin2].as_decimal(6))
         print("alum:", mod[alum].as_decimal(6), "silicon:", mod[silicon].as_decimal(6))
         print()

plan()
