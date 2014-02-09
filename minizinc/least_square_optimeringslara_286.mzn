% 
% Least square in Minizinc.
%
% From the swedish book "Optimeringslara", page 286f
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
int: num;                  % number of points 
array[1..num] of float: t;   % temperature
array[1..num] of float: F; % percentage gas

array[0..4] of var -100.0..100.0: a; % equation


solve :: float_search(a,0.0001, input_order,indomain_split,complete) 
     minimize sum(i in 1..num) (
       % F[i] - (a[0] + a[1]*t[i] + a[2]*t[i]*t[i] + a[3]*t[i]*t[i]*t[i] + a[4]*t[i]*t[i]*t[i]*t[i])
       F[i] - (a[0] + a[1]*t[i] + a[2]*pow(t[i],2.0) + a[3]*pow(t[i],3.0) + a[4]*pow(t[i],4.0))
   )
;

constraint 
  a[0] + 20.0*a[1] + 20.0*20.0*a[2] + 20.0*20.0*20.0*a[3] + 20.0*20.0*20.0*20.0*a[4] = 0.0
  /\
  a[0] + 700.0*a[1] + 700.0*700.0*a[2] + 700.0*700.0*700.0*a[3] + 700.0*700.0*700.0*700.0*a[4] = 100.0
  /\
  forall(i in 1..num) (
         % (a[1] + 2.0*a[2]*t[i] + 3.0*a[3]*t[i]*t[i] + 4.0*a[4]*t[i]*t[i]*t[i]) >= 0.0
        (a[1] + 2.0*a[2]*t[i] + 3.0*a[3]*pow(t[i],2.0) + 4.0*a[4]*pow(t[i],3.0)) >= 0.0
  )
;

%
% data;
%
num = 14;
t = [20.0, 30.0, 80.0,125.0,175.0,225.0,275.0,325.0,360.0,420.0,495.0,540.0,630.0,700.0];
F = [0.0,5.8 ,14.7,31.6,43.2,58.3,78.4,89.4,96.4,99.1,99.5,99.9,100.0,100.0]; 


output [show(a)];