# 
# Conversion Celsius <-> Fahrenheit in z3.
#
# This is a standard example in constraint programming showing the
# "reversibility" of a model: i.e. a single predicate can convert from
# Celsius to Fahrenheit or from Fahrenheit to Celsius.
# 
# This z3 model was created by Hakan Kjellerstrand, hakank@gmail.com
# See also my z3 page: http://www.hakank.org/z3/
#

from z3 import *

def convert(c,f):
    return c == (f - 32.0) * 5.0 / 9.0

def test():
    s = Solver()

    c = Real("c")
    f = Real("f")

    s.add(convert(c,f))
    s.add(Or(c == 0, f == 0, c == 100, f == 100, c == 1000, f == 1000))
    
    while s.check() == sat:
        mod = s.model()
        print("c:", mod[c], mod[c].as_decimal(10))
        print("f:", mod[f], mod[f].as_decimal(10))
        print()
        s.add(Or(c != mod[c], f != mod[f]))
        
  

test()
