# 
# Curve fitting problem in z3.
#
#
#
# This z3 model was created by Hakan Kjellerstrand, hakank@gmail.com
# See also my z3 page: http://www.hakank.org/z3/
#

from z3 import *


#
# From GLPK example cf12a.mod:
# """
# Curve fitting problem 12.11(a) H P Williams "Model Building in 
# Mathematical Programming"
# """
#
# The answer should be 
#    y = 0.6375x + 0.581
#
def curve_fitting1(x,y):
    print("\bCurve fitting 1")
    n = len(x)
    
    s = Optimize()
    
    a,b,z = Reals("a b z")
    u = [Real(f"u{i}") for i in range(n)]
    v = [Real(f"v{i}") for i in range(n)]

    s.add(a >= 0, a <= 1,
          b >= 0, b <= 1,
          z >= 0, z <= 30,
          z == Sum(u) + Sum(v) )
    
    for i in range(n):
        s.add(u[i] >= 0.0, u[i] <= 3.0,
              v[i] >= 0.0, v[i] <= 3.0,              
              b * x[i] + a + u[i] - v[i] == y[i],
              )

    s.minimize(z)
    
    if s.check() == sat:
        mod = s.model()
        print(f"a:{mod[a].as_decimal(6)} b:{mod[b].as_decimal(6)} z:{mod[z].as_decimal(6)}")
        print("u:", [mod[u[i]].as_decimal(3) for i in range(n)])
        print("v:", [mod[v[i]].as_decimal(3) for i in range(n)])
        print(f"\ny = {mod[b].as_decimal(6)}*x + {mod[a].as_decimal(6)} deviation: {mod[z].as_decimal(6)}")
        print()


# From GLPK example cf12b.mod:
# """
# Curve fitting problem 12.11(b) 
# H P Williams "Model Building in Mathematical Programming"
# """
# The answer should be 
#  y = 0.6250x + -0.4000 Max deviation = 1.7250
#
def curve_fitting2(x,y):
    print("\bCurve fitting 2")
    n = len(x)
    
    s = Optimize()
    
    a,b,z = Reals("a b z")
    u = [Real(f"u{i}") for i in range(n)]
    v = [Real(f"v{i}") for i in range(n)]
    
    for i in range(n):
        s.add(u[i] >= 0.0,
              v[i] >= 0.0,
              b * x[i] + a + u[i] - v[i] == y[i],
              # deviation constraints 
              z - u[i] >= 0.0,
              z - v[i] >= 0.0
              )

    s.minimize(z)
    
    if s.check() == sat:
        mod = s.model()
        print(f"a:{mod[a].as_decimal(6)} b:{mod[b].as_decimal(6)} z:{mod[z].as_decimal(6)}")
        print("u:", [mod[u[i]].as_decimal(3) for i in range(n)])
        print("v:", [mod[v[i]].as_decimal(3) for i in range(n)])
        print(f"\ny = {mod[b].as_decimal(6)}*x + {mod[a].as_decimal(6)} max deviation: {mod[z].as_decimal(6)}")
        print()
        # s.add(z < mod[z])

#
# From GLPK example cflsq.mod
# """
# Curve fitting problem by Least Squares
# """
# The answer should be 
#    y = 0.426126 + 0.610772x
#
def curve_fitting3(sx,sy):
    print("\bCurve fitting 3")
    
    n = len(sx)

    # Note: This is not an optimization problem
    s = Solver()
    
    x, y, b1 = Reals("x y b1")
    ex = [Real(f"ex{i}") for i in range(n)]
    ey = [Real(f"ey{i}") for i in range(n)]
    t = Real("t")

    # sum of variances is zero for Sx
    s.add(Sum(ex) == 0.0)
    for i in range(n):
        s.add(x + ex[i] == sx[i])
        
    # sum of variances is zero for Sy
    s.add(Sum(ey) == 0.0)
    for i in range(n):
        s.add(y + ey[i] == sy[i])

    s.add(b1 == Sum([ex[i]*ey[i] for i in range(n)])/Sum([ex[i]*ex[i] for i in range(n)]))
    s.add(t == y-b1*x)

    # s.minimize(z)
    
    if s.check() == sat:
        mod = s.model()
        print(f"x:{mod[x].as_decimal(6)} y:{mod[y].as_decimal(6)} b1:{mod[b1].as_decimal(6)} t:{mod[t].as_decimal(6)}")
        print("ex:", [mod[ex[i]].as_decimal(3) for i in range(n)])
        print("ey:", [mod[ey[i]].as_decimal(3) for i in range(n)])
        print(f"\ny = {mod[t].as_decimal(6)}*x + {mod[b1].as_decimal(6)}")
        print()



x = [0.0, 0.5,1.0,1.5,1.9,2.5,3.0,3.5,4.0,4.5,5.0,5.5,6.0,6.6,7.0,7.6,8.5,9.0,10.0]
y = [1.0,0.9,0.7,1.5,2.0,2.4,3.2,2.0,2.7,3.5,1.0,4.0,3.6,2.7,5.7,4.6,6.0,6.8,7.3]
curve_fitting1(x,y)
print()
curve_fitting2(x,y)
print()
curve_fitting3(x,y)
