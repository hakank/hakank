% 
% Number puzzle in MiniZinc.
% 
% From Microsoft Solver Foundation discussion
% http://code.msdn.microsoft.com/solverfoundation/Thread/View.aspx?ThreadId=1062
% """
% I am trying to solve a number puzzle using Microsoft Solver Foundation via Excel.
% I have successfully defined the Decisions and started to define the Constraints.
% The puzzle has now been solving for 36 hours and has not yet found a solution.
% Is there anything I can do to see how long it might take to solve?
% Is there a way to improve my Model so that it can be solved faster?
% Thanks!
% """

% This was mentioned in Erwin Kalvelagen's blog post
% "MSF CSP: Domain Over Weighted Degree"
% http://yetanothermathprogrammingconsultant.blogspot.com/2008/12/msf-csp-domain-over-weighted-degree.html


%
% Note: I don't know what the problem really is but it seems to be an 
% alphametic problem of some kind. There are _alot_ of solutions.
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

% I don't use set of for a..z, rad01..rad40 since the model gets quite big
% (it enumerates all the number in the range). Instead these range is stated
% explicitly in the constraint section.
%
% set of int: R = 10000..99999;
var int:  a;
var int:  b;
var int:  c;
var int:  d;
var int:  e;
var int:  f;
var int:  g;
var int:  h;
var int:  i;
var int:  j;
var int:  k;
var int:  l;
var int:  m;
var int:  n;
var int:  o;
var int:  p;
var int:  q;
var int:  r;
var int:  s;
var int:  t;
var int:  u;
var int:  v;
var int:  w;
var int:  x;
var int:  y;
var int:  z;
var int:  rad01;
var int:  rad02;
var int:  rad03;
var int:  rad04;
var int:  rad05;
var int:  rad06;
var int:  rad07;
var int:  rad08;
var int:  rad09;
var int:  rad10;
var int:  rad11;
var int:  rad12;
var int:  rad13;
var int:  rad14;
var int:  rad15;
var int:  rad16;
var int:  rad17;
var int:  rad18;
var int:  rad19;
var int:  rad20;
var int:  rad21;
var int:  rad22;
var int:  rad23;
var int:  rad24;
var int:  rad25;
var int:  rad26;
var int:  rad27;
var int:  rad28;
var int:  rad29;
var int:  rad30;
var int:  rad31;
var int:  rad32;
var int:  rad33;
var int:  rad34;
var int:  rad35;
var int:  rad36;
var int:  rad37;
var int:  rad38;
var int:  rad39;
var int:  rad40;

set of int: S = 1..9;
var S: a1; 
var S: b1; 
var S: c1; 
var S: d1; 
var S: e1; 
var S: f1; 
var S: g1; 
var S: h1; 
var S: i1; 
var S: j1; 
var S: k1; 
var S: l1; 
var S: m1; 
var S: n1; 
var S: o1; 
var S: p1; 
var S: q1; 
var S: r1; 
var S: s1; 
var S: t1; 
var S: u1; 
var S: v1; 
var S: w1; 
var S: x1; 
var S: y1; 
var S: z1;

set of int: T = 0..9;
var T: a2; 
var T: b2; 
var T: c2; 
var T: d2;
var T: e2;
var T: f2;
var T: g2;
var T: h2;
var T: i2;
var T: j2;
var T: k2;
var T: l2;
var T: m2;
var T: n2;
var T: o2;
var T: p2;
var T: q2;
var T: r2;
var T: s2;
var T: t2;
var T: u2;
var T: v2;
var T: w2;
var T: x2;
var T: y2;
var T: z2;
var T: a3;
var T: b3;
var T: c3;
var T: d3;
var T: e3;
var T: f3;
var T: g3;
var T: h3;
var T: i3;
var T: j3;
var T: k3;
var T: l3;
var T: m3;
var T: n3;
var T: o3;
var T: p3;
var T: q3;
var T: r3;
var T: s3;
var T: t3;
var T: u3;
var T: v3;
var T: w3;
var T: x3;
var T: y3;
var T: z3;
var T: a4;
var T: b4;
var T: c4;
var T: d4;
var T: e4;
var T: f4;
var T: g4;
var T: h4;
var T: i4;
var T: j4;
var T: k4;
var T: l4;
var T: m4;
var T: n4;
var T: o4;
var T: p4;
var T: q4;
var T: r4;
var T: s4;
var T: t4;
var T: u4;
var T: v4;
var T: w4;
var T: x4;
var T: y4;
var T: z4;
var T: a5;
var T: b5;
var T: c5;
var T: d5;
var T: e5;
var T: f5;
var T: g5;
var T: h5;
var T: i5;
var T: j5;
var T: k5;
var T: l5;
var T: m5;
var T: n5;
var T: o5;
var T: p5;
var T: q5;
var T: r5;
var T: s5;
var T: t5;
var T: u5;
var T: v5;
var T: w5;
var T: x5;
var T: y5;
var T: z5;



% "occurrence" is very fast
solve :: int_search([
a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,

rad01,rad02,rad03,rad04,rad05,rad06,rad07,rad08,rad09,
rad10,rad11,rad12,rad13,rad14,rad15,rad16,rad17,rad18,rad19,
rad20,rad21,rad22,rad23,rad24,rad25,rad26,rad27,rad28,rad29,
rad30,rad31,rad32,rad33,rad34,rad35,rad36,rad37,rad38,rad39,rad40,

a1,a2,a3,a4,a5,  
b1,b2,b3,b4,b5,  
c1,c2,c3,c4,c5,  
d1,d2,d3,d4,d5,  
e1,e2,e3,e4,e5,  
f1,f2,f3,f4,f5,  
g1,g2,g3,g4,g5,  
h1,h2,h3,h4,h5,  
i1,i2,i3,i4,i5,  
j1,j2,j3,j4,j5,  
k1,k2,k3,k4,k5,  
l1,l2,l3,l4,l5,  
m1,m2,m3,m4,m5,  
n1,n2,n3,n4,n5,  
o1,o2,o3,o4,o5,  
p1,p2,p3,p4,p5,  
q1,q2,q3,q4,q5,  
r1,r2,r3,r4,r5,  
s1,s2,s3,s4,s5,  
t1,t2,t3,t4,t5,  
u1,u2,u3,u4,u5,  
v1,v2,v3,v4,v5,  
w1,w2,w3,w4,w5,  
x1,x2,x3,x4,x5,  
y1,y2,y3,y4,y5,  
z1,z2,z3,z4,z5,  

], 
 occurrence, % first_fail, % occurrence, 
 indomain_min, 
 complete) satisfy;

constraint
 
a = a1*10000+a2*1000+a3*100+a4*10+a5 /\ 
b = b1*10000+b2*1000+b3*100+b4*10+b5 /\ 
c = c1*10000+c2*1000+c3*100+c4*10+c5 /\ 
d = d1*10000+d2*1000+d3*100+d4*10+d5 /\ 
e = e1*10000+e2*1000+e3*100+e4*10+e5 /\ 
f = f1*10000+f2*1000+f3*100+f4*10+f5 /\ 
g = g1*10000+g2*1000+g3*100+g4*10+g5 /\ 
h = h1*10000+h2*1000+h3*100+h4*10+h5 /\ 
i = i1*10000+i2*1000+i3*100+i4*10+i5 /\ 
j = j1*10000+j2*1000+j3*100+j4*10+j5 /\ 
k = k1*10000+k2*1000+k3*100+k4*10+k5 /\ 
l = l1*10000+l2*1000+l3*100+l4*10+l5 /\ 
m = m1*10000+m2*1000+m3*100+m4*10+m5 /\ 
n = n1*10000+n2*1000+n3*100+n4*10+n5 /\ 
o = o1*10000+o2*1000+o3*100+o4*10+o5 /\ 
p = p1*10000+p2*1000+p3*100+p4*10+p5 /\ 
q = q1*10000+q2*1000+q3*100+q4*10+q5 /\ 
r = r1*10000+r2*1000+r3*100+r4*10+r5 /\ 
s = s1*10000+s2*1000+s3*100+s4*10+s5 /\ 
t = t1*10000+t2*1000+t3*100+t4*10+t5 /\ 
u = u1*10000+u2*1000+u3*100+u4*10+u5 /\ 
v = v1*10000+v2*1000+v3*100+v4*10+v5 /\ 
w = w1*10000+w2*1000+w3*100+w4*10+w5 /\ 
x = x1*10000+x2*1000+x3*100+x4*10+x5 /\ 
y = y1*10000+y2*1000+y3*100+y4*10+y5 /\ 
z = z1*10000+z2*1000+z3*100+z4*10+z5 /\ 
 
rad01 = sum([h5*1000,i4*1000,q2*100,u4*10,y1]) /\ 
rad02 = sum([a1*1000,i5*1000,q2*100,u4*10,y1]) /\ 
rad03 = sum([a2*1000,j1*1000,q3*100,u5*10,y1]) /\ 
rad04 = sum([a3*1000,j2*1000,q3*100,u5*10,y1]) /\ 
rad05 = sum([a4*1000,j3*1000,q4*100,v1*10,y2]) /\ 
rad06 = sum([a5*1000,j4*1000,q4*100,v1*10,y2]) /\ 
rad07 = sum([b1*1000,j5*1000,q5*100,v2*10,y2]) /\ 
rad08 = sum([b2*1000,k1*1000,q5*100,v2*10,y2]) /\ 
rad09 = sum([b3*1000,k2*1000,r1*100,v3*10,y3]) /\ 
rad10 = sum([b4*1000,k3*1000,r1*100,v3*10,y3]) /\ 
rad11 = sum([b5*1000,k4*1000,r2*100,v4*10,y3]) /\ 
rad12 = sum([c1*1000,k5*1000,r2*100,v4*10,y3]) /\ 
rad13 = sum([c2*1000,l1*1000,r3*100,v5*10,y4]) /\ 
rad14 = sum([c3*1000,l2*1000,r3*100,v5*10,y4]) /\ 
rad15 = sum([c4*1000,l3*1000,r4*100,w1*10,y4]) /\ 
rad16 = sum([c5*1000,l4*1000,r4*100,w1*10,y4]) /\ 
rad17 = sum([d1*1000,l5*1000,r5*100,w2*10,y5]) /\ 
rad18 = sum([d2*1000,m1*1000,r5*100,w2*10,y5]) /\ 
rad19 = sum([d3*1000,m2*1000,s1*100,w3*10,y5]) /\ 
rad20 = sum([d4*1000,m3*1000,s1*100,w3*10,y5]) /\ 
rad21 = sum([d5*1000,m4*1000,s2*100,w4*10,z1]) /\ 
rad22 = sum([e1*1000,m5*1000,s2*100,w4*10,z1]) /\ 
rad23 = sum([e2*1000,n1*1000,s3*100,w5*10,z1]) /\ 
rad24 = sum([e3*1000,n2*1000,s3*100,w5*10,z1]) /\ 
rad25 = sum([e4*1000,n3*1000,s4*100,x1*10,z2]) /\ 
rad26 = sum([e5*1000,n4*1000,s4*100,x1*10,z2]) /\ 
rad27 = sum([f1*1000,n5*1000,s5*100,x2*10,z2]) /\ 
rad28 = sum([f2*1000,o1*1000,s5*100,x2*10,z2]) /\ 
rad29 = sum([f3*1000,o2*1000,t1*100,x3*10,z3]) /\ 
rad30 = sum([f4*1000,o3*1000,t1*100,x3*10,z3]) /\ 
rad31 = sum([f5*1000,o4*1000,t2*100,x4*10,z3]) /\ 
rad32 = sum([g1*1000,o5*1000,t2*100,x4*10,z3]) /\ 
rad33 = sum([g2*1000,p1*1000,t3*100,x5*10,z4]) /\ 
rad34 = sum([g3*1000,p2*1000,t3*100,x5*10,z4]) /\ 
rad35 = sum([g4*1000,p3*1000,t4*100,u1*10,z4]) /\ 
rad36 = sum([g5*1000,p4*1000,t4*100,u1*10,z4]) /\ 
rad37 = sum([h1*1000,p5*1000,t5*100,u2*10,z5]) /\ 
rad38 = sum([h2*1000,i1*1000,t5*100,u2*10,z5]) /\
rad39 = sum([h3*1000,i2*1000,q1*100,u3*10,z5]) /\
rad40 = sum([h4*1000,i3*1000,q1*100,u3*10,z5]) /\

all_different([
a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,
rad01,rad02,rad03,rad04,rad05,rad06,rad07,rad08,rad09,
rad10,rad11,rad12,rad13,rad14,rad15,rad16,rad17,rad18,rad19,
rad20,rad21,rad22,rad23,rad24,rad25,rad26,rad27,rad28,rad29,
rad30,rad31,rad32,rad33,rad34,rad35,rad36,rad37,rad38,rad39,rad40])
/\

% See note above. 
a >= 10000 /\ a <= 99999 /\
b >= 10000 /\ b <= 99999 /\
c >= 10000 /\ c <= 99999 /\
d >= 10000 /\ d <= 99999 /\
e >= 10000 /\ e <= 99999 /\
f >= 10000 /\ f <= 99999 /\
g >= 10000 /\ h <= 99999 /\
h >= 10000 /\ h <= 99999 /\
i >= 10000 /\ i <= 99999 /\
j >= 10000 /\ j <= 99999 /\
k >= 10000 /\ k <= 99999 /\
l >= 10000 /\ l <= 99999 /\
m >= 10000 /\ m <= 99999 /\
n >= 10000 /\ n <= 99999 /\
o >= 10000 /\ o <= 99999 /\
p >= 10000 /\ p <= 99999 /\
q >= 10000 /\ q <= 99999 /\
r >= 10000 /\ r <= 99999 /\
s >= 10000 /\ s <= 99999 /\
t >= 10000 /\ t <= 99999 /\
u >= 10000 /\ u <= 99999 /\
v >= 10000 /\ v <= 99999 /\
w >= 10000 /\ w <= 99999 /\
x >= 10000 /\ x <= 99999 /\
y >= 10000 /\ y <= 99999 /\
z >= 10000 /\ z <= 99999 /\

rad01 >= 10000 /\ rad01 <= 99999 /\
rad02 >= 10000 /\ rad02 <= 99999 /\
rad03 >= 10000 /\ rad03 <= 99999 /\
rad04 >= 10000 /\ rad04 <= 99999 /\
rad05 >= 10000 /\ rad05 <= 99999 /\
rad06 >= 10000 /\ rad06 <= 99999 /\
rad07 >= 10000 /\ rad07 <= 99999 /\
rad08 >= 10000 /\ rad08 <= 99999 /\
rad09 >= 10000 /\ rad09 <= 99999 /\
rad10 >= 10000 /\ rad10 <= 99999 /\
rad11 >= 10000 /\ rad11 <= 99999 /\
rad12 >= 10000 /\ rad12 <= 99999 /\
rad13 >= 10000 /\ rad13 <= 99999 /\
rad14 >= 10000 /\ rad14 <= 99999 /\
rad15 >= 10000 /\ rad15 <= 99999 /\
rad16 >= 10000 /\ rad16 <= 99999 /\
rad17 >= 10000 /\ rad17 <= 99999 /\
rad18 >= 10000 /\ rad18 <= 99999 /\
rad19 >= 10000 /\ rad19 <= 99999 /\
rad20 >= 10000 /\ rad20 <= 99999 /\
rad21 >= 10000 /\ rad21 <= 99999 /\
rad22 >= 10000 /\ rad22 <= 99999 /\
rad23 >= 10000 /\ rad23 <= 99999 /\
rad24 >= 10000 /\ rad24 <= 99999 /\
rad25 >= 10000 /\ rad25 <= 99999 /\
rad26 >= 10000 /\ rad26 <= 99999 /\
rad27 >= 10000 /\ rad27 <= 99999 /\
rad28 >= 10000 /\ rad28 <= 99999 /\
rad29 >= 10000 /\ rad29 <= 99999 /\
rad30 >= 10000 /\ rad30 <= 99999 /\
rad31 >= 10000 /\ rad31 <= 99999 /\
rad32 >= 10000 /\ rad32 <= 99999 /\
rad33 >= 10000 /\ rad33 <= 99999 /\
rad34 >= 10000 /\ rad34 <= 99999 /\
rad35 >= 10000 /\ rad35 <= 99999 /\
rad36 >= 10000 /\ rad36 <= 99999 /\
rad37 >= 10000 /\ rad37 <= 99999 /\
rad38 >= 10000 /\ rad38 <= 99999 /\
rad39 >= 10000 /\ rad39 <= 99999 /\
rad40 >= 10000 /\ rad40 <= 99999

;


output [
  "\na..z: ", show([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]), "\n",
  "rad01..rad40: ", show([
      rad01,rad02,rad03,rad04,rad05,rad06,rad07,rad08,rad09,
rad10,rad11,rad12,rad13,rad14,rad15,rad16,rad17,rad18,rad19,
rad20,rad21,rad22,rad23,rad24,rad25,rad26,rad27,rad28,rad29,
rad30,rad31,rad32,rad33,rad34,rad35,rad36,rad37,rad38,rad39,
rad40]), "\n", 
  "a1..z5: ", show([
a1,a2,a3,a4,a5,  
b1,b2,b3,b4,b5,  
c1,c2,c3,c4,c5,  
d1,d2,d3,d4,d5,  
e1,e2,e3,e4,e5,  
f1,f2,f3,f4,f5,  
g1,g2,g3,g4,g5,  
h1,h2,h3,h4,h5,  
i1,i2,i3,i4,i5,  
j1,j2,j3,j4,j5,  
k1,k2,k3,k4,k5,  
l1,l2,l3,l4,l5,  
m1,m2,m3,m4,m5,  
n1,n2,n3,n4,n5,  
o1,o2,o3,o4,o5,  
p1,p2,p3,p4,p5,  
q1,q2,q3,q4,q5,  
r1,r2,r3,r4,r5,  
s1,s2,s3,s4,s5,  
t1,t2,t3,t4,t5,  
u1,u2,u3,u4,u5,  
v1,v2,v3,v4,v5,  
w1,w2,w3,w4,w5,  
x1,x2,x3,x4,x5,  
y1,y2,y3,y4,y5,  
z1,z2,z3,z4,z5]), "\n",

];