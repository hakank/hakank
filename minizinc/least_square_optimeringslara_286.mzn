% 
% Least square in Minizinc.
%
% From the swedish book "Optimeringslära", page 286f
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
int: num;                  % number of points 
array[1..num] of int: t;   % temperature
array[1..num] of float: F; % percentage gas

array[0..4] of var float: a; % equation


solve minimize sum(i in 1..num) (
       F[i] - (a[0] + a[1]*int2float(t[i]) + a[2]*int2float(t[i])*int2float(t[i]) + a[3]*int2float(t[i])*int2float(t[i])*int2float(t[i]) + a[4]*int2float(t[i])*int2float(t[i])*int2float(t[i])*int2float(t[i]))
   )
;

constraint 
  a[0] + 20.0*a[1] + 20.0*20.0*a[2] + 20.0*20.0*20.0*a[3] + 20.0*20.0*20.0*20.0*a[4] = 0.0
  /\
  a[0] + 700.0*a[1] + 700.0*700.0*a[2] + 700.0*700.0*700.0*a[3] + 700.0*700.0*700.0*700.0*a[4] = 100.0
  /\
  forall(i in 1..num) (
         (a[1] + 2.0*a[2]*int2float(t[i]) + 3.0*a[3]*int2float(t[i])*int2float(t[i]) + 4.0*a[4]*int2float(t[i])*int2float(t[i])*int2float(t[i])) >= 0.0
  )
;

%
% data;
%
num = 14;
t = [20, 30, 80,125,175,225,275,325,360,420,495,540,630,700];
F = [0.0,5.8 ,14.7,31.6,43.2,58.3,78.4,89.4,96.4,99.1,99.5,99.9,100.0,100.0]; 


output [show(a)];