%
% Stock model in Minizinc
%
% From http://www.mai.liu.se/~olbur/kurser/NMAB18/labinfo_1.pdf
% (AMPL model, Swedish)
%
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: T;                % num time periods
int: initstock;        % initial stock
array[1..T] of int: c; % production cost 
array[1..T] of int: u; % production capacity
array[1..T] of int: l; % stock cost
array[1..T] of int: d; % demand

array[1..T] of var int: x; % num manufactured units
array[0..T] of var int: y; % num units in stock

solve minimize sum (t in 1..T) (c[t]*x[t] + l[t]*y[t]);

constraint 
  forall(t in 1..T) ( 
     x[t] >= 0 /\
     y[t] >= 0
  ) 
  /\
  forall(t in 1..T) (
     y[t-1] + x[t] = d[t] + y[t]
  )
  /\
  y[0] = initstock
  /\
  forall(t in 1..T) (
   x[t] <= u[t]
  )
;

%
% data
%
T = 3;
initstock = 5;

c = [25, 36, 30];
l = [4, 3, 5];
d = [10, 15,12];
u = [20, 16, 10];

output
[
  "x: " ++show(x) ++ "\n" ++
  "y: " ++show(y) ++ "\n"
];
