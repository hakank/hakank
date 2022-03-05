#  
# Agricultural pricing in MiniZinc.
#  
# From H. Paul Williams "Model Building in Mathematical Programming", 4th edition
# Agricultural Pricing example, sections 12.21, 13.21, 14.21
#
# Output:
# revenue: 1991.667196443451?
# q: -0.95
# milk price:  303.0429159905593
# butter price:  666.7676403921997
# cheese 1 price:  900.0
# cheese 2 price:  1085.25
# milk prod:  4780.771912357581
# butter prod:  383.8788315293602
# cheese 1 prod:  249.9634969325153
# cheese 2 prod:  56.71533742331288
#  
#  This z3 model was created by Hakan Kjellerstrand, hakank@gmail.com
#  See also my z3 page: http://www.hakank.org/z3
# 

from z3 import *


def agprice():
    s = Optimize()

    grid = [0.06,0.1,0.15,0.2,0.25,0.3,0.3125,0.325,0.35,0.4,0.45,0.5,0.55,
            0.6,0.65,0.6625,0.66875,0.7,0.75,0.8,0.85,0.9,0.95,1.0,1.025,
            1.05,1.1,1.15,1.2,1.25,1.3,1.35,1.4,1.45,1.5]
    n_point = len(grid)

    milk,milksq,butt,buttsq,cha,chasq,chb,chbsq,xm,xb,xca,xcb,q,qsq = Reals("milk milksq butt buttsq cha chasq chb chbsq xm xb xca xcb q qsq")

    s.add(milk >= 0.0,
          milksq >= 0.0,
          butt >= 0.0,
          buttsq >= 0.0,
          cha >= 0.0,
          chasq >= 0.0,
          chb >= 0.0,
          chbsq >= 0.0,
          xm >= 0.0,
          xb >= 0.0,
          xca >= 0.0,
          xcb >= 0.0,
          # q >= 0.0, # q is free
          qsq >= 0.0
             )


    lmilk = [Real(f"lmilk[{i}]") for i in range(n_point)]
    lbutt = [Real(f"lbutt[{i}]") for i in range(n_point)]
    lcha  = [Real(f"lcha[{i}]") for i in range(n_point)]
    lchb  = [Real(f"lchb[{i}]") for i in range(n_point)]
    lq    = [Real(f"lq[{i}]") for i in range(n_point)]
    mq    = [Real(f"mq[{i}]") for i in range(n_point)]
    for i in range(n_point):
        s.add(lmilk[i] >= 0, lmilk[i] <= 1,
              lbutt[i] >= 0, lbutt[i] <= 1,
              lcha[i] >= 0, lcha[i] <= 1,
              lchb[i] >= 0, lchb[i] <= 1,
              lq[i] >= 0, lq[i] <= 1,
              mq[i] >= 0, mq[i] <= 1
              )

    s.add(Sum(lmilk) >= 0, Sum(lmilk) <= 1)
    s.add(Sum(lbutt) >= 0, Sum(lbutt) <= 1)
    s.add(Sum(lcha) >= 0, Sum(lcha) <= 1)
    s.add(Sum(lchb) >= 0, Sum(lchb) <= 1)
    s.add(Sum([mq[i]+lq[i] for i in range(n_point)]) <= 1.0)

    # to maximize
    revenue = Real("revenue")
    s.add(revenue == -6492.0*milksq-1200.0*buttsq-194.0*chasq-8.0*chbsq-qsq +6748.0*milk+1185.0*butt+420.0*cha+70.0*chb)
    s.maximize(revenue)

    
    s.add(
        (1.0/4.82)*xm+(0.4/0.297)*milk == 1.4,
        (1.0/0.32)*xb+(2.7/0.720)*butt == 3.7,
        (1.0/0.21)*xca+(1.1/1.05)*cha -(0.1/0.815)*chb == 2.0,
        (1.0/0.07)*xcb+(0.4/0.815)*chb -(0.4/1.05)*cha == 1.0,
        0.04*xm+0.8*xb+0.35*xca+0.25*xcb<=0.600,
        0.09*xm+0.02*xb+0.3*xca+0.4*xcb<=0.750,
        4.82*milk+0.32*butt+0.21*cha+0.07*chb <= 1.939,
        cha-chb-0.195*q == 0.0,
        milk - Sum([grid[i]*lmilk[i] for i in range(n_point)]) ==0.0,
        butt - Sum([grid[i]*lbutt[i] for i in range(n_point)])==0.0,
        cha - Sum([grid[i]*lcha[i] for i in range(n_point)])==0.0,
        chb - Sum([grid[i]*lchb[i] for i in range(n_point)])==0.0,
        q + Sum([grid[i]*mq[i] for i in range(n_point)])- Sum([grid[i]*lq[i] for i in range(n_point)])==0.0,
        milksq-Sum([grid[i]*grid[i] * lmilk[i] for i in range(n_point)])==0.0,
        buttsq-Sum([grid[i]*grid[i] * lbutt[i] for i in range(n_point)])==0.0,
        chasq-Sum([grid[i]*grid[i] * lcha[i] for i in range(n_point)])==0.0,
        chbsq-Sum([grid[i]*grid[i] * lchb[i] for i in range(n_point)])==0.0,
        qsq-Sum([grid[i]*grid[i] * mq[i] for i in range(n_point)])- Sum([grid[i]*grid[i] * lq[i] for i in range(n_point)])==0.0
        )

    s.add(revenue >= 0)
    

    if s.check() == sat:
        mod = s.model()
        print("revenue:", mod[revenue].as_decimal(12))
        print("q:", mod[q].as_decimal(12))        
        print("milk price: ", mod[milk].as_fraction()*1000.0)
        print("butter price: ", mod[butt].as_fraction()*1000.0)
        print("cheese 1 price: ", mod[cha].as_fraction()*1000.0)
        print("cheese 2 price: ", mod[chb].as_fraction()*1000.0)
        print("milk prod: ", mod[xm].as_fraction()*1000.0)
        print("butter prod: ", mod[xb].as_fraction()*1000.0)
        print("cheese 1 prod: ", mod[xca].as_fraction()*1000.0)
        print("cheese 2 prod: ", mod[xcb].as_fraction()*1000.0)
        print()
        # s.add(revenue > mod[revenue]+1) # take some larger steps

    
agprice()
