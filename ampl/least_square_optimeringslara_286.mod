/*
  Optimeringslarea page 286f

  Least square for a fourth degree equation


MINOS:
0  -17.2749
1    0.915793
2   -0.00266941
3    3.38763e-06
4   -1.5732e-09


   This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
   See also my AMPL page: http://www.hakank.org/ampl/


*/

param antal; 
param t{1..antal}; 
param F{1..antal}; 

var a{0..4}; 

minimize z:
        sum{i in 1..antal} (F[i] - (a[0] + a[1]*t[i] + a[2]*t[i]^2 + a[3]*t[i]^3 + a[4]*t[i]^4 ))
;

# 
s.t. c1:
     a[0] + 20*a[1] + 20^2*a[2] + 20^3*a[3] + 20^4*a[4] = 0;


# 
s.t. c2:
     a[0] + 700*a[1] + 700^2*a[2] + 700^3*a[3] + 700^4*a[4] = 100;

# 
s.t. c3{i in 1..antal}:
         (a[1] + 2*a[2]*t[i] + 3*a[3]*t[i]^2 + 4*a[4]*t[i]^3) >= 0;

data;

param antal := 14;
param: t F :=
        1   20 0
        2   30 5.8 
        3   80 14.7
        4  125 31.6
        5  175 43.2
        6  225 58.3
        7  275 78.4
        8  325 89.4
        9  360 96.4
        10 420 99.1
        11 495 99.5
        12 540 99.9
        13 630 100
        14 700 100
; 


# option solver cplex;
#option solver snopt;

solve;

display a, _obj;
