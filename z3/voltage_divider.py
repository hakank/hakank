#
# Voltage divider in z3.
#
# Problem from Marriott & Stuckey Programming in Constraints, page 137ff
#
# This Z3 model was written by Hakan Kjellerstrand (hakank@gmail.com)
# See also my Z3 page: http://hakank.org/z3/
#

from z3 import *


def voltage_divider(v, i, r1, r2, vd, idd):
    v1, i2 = Reals("v1 i2")
    return [v1 == i * r1,
            vd == i2 * r2,
            v == v1 + vd,
            i == i2 + idd]

# cells = [9.0, 12.0]  
def cell(c):
    return Or(c==9, c==12)

# resistors =  [ 5.0, 9.0, 14.0 ]   
def resistor(r):
    return Or(r == 5, r == 9, r == 14)

def voltage_divider_test():
    s = Solver()
    
    # Goal for voltage divider program page 138.
    v, r1, r2, vd, idd, i = Reals("v r1 r2 vd idd i")
    
    s.add(voltage_divider(v, i, r1, r2, vd, idd))
    s.add(5.4 <= vd,
          vd <= 5.5,
          idd == 0.1
          )
    s.add(cell(v))
    s.add(resistor(r1))
    s.add(resistor(r2))

    while s.check() == sat:
        mod = s.model()
        print("v:", mod[v], mod[v].as_decimal(6))
        print("r1:", mod[r1], mod[r1].as_decimal(6))
        print("r2:", mod[r2], mod[r2].as_decimal(6))
        print("vd:", mod[vd], mod[vd].as_decimal(6))
        print("id:", mod[idd], mod[idd].as_decimal(6))
        print("i:", mod[i], mod[i].as_decimal(6))
        s.add(Or(v != mod[v], r1 != mod[r1], r2 != mod[r2],
                 vd != mod[vd], idd != mod[idd], i != mod[i]))

        
voltage_divider_test()
