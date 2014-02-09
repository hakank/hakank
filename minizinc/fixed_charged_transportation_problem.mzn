% 
% Fixed charged transportation problem in MiniZinc.
% 
% From the GLPK example fctp.mod
% """
% FCTP, Fixed-Charge Transportation Problem
%
% Written in GNU MathProg by Andrew Makhorin <mao@mai2.rcnet.ru>
% 
% The Fixed-Charge Transportation Problem (FCTP) is obtained from
% classical transportation problem by imposing a fixed cost on each
% transportation link if there is a positive flow on that link.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% number of sources 
int: m;

% number of customers 
int: n;

% set of sources 
set of int: I = 1..m;

% set of customers 
set of int: J = 1..n;

% supply at source i 
array[I] of float: supply;

% demand at customer j 
array[J] of float: demand;

% variable cost (a cost per one unit shipped from i to j) 
array[I,J] of float: varcost;

% fixed cost (a cost for shipping any amount from i to j) 
array[I,J] of float: fixcost;

% amount shipped from source i to customer j 
array[I,J] of var float: x;

% y[i,j] = 1 means some amount is shipped from i to j 
array[I,J] of var 0..1: y;


% total transportation costs 
var float: cost  = sum(i in I, j in J) (varcost[i,j] * x[i,j]) +
                  sum(i in I, j in J) (fixcost[i,j] * int2float(y[i,j]));

% solve minimize cost;
solve :: int_search([y[i,j] | i in I, j in J], first_fail, indomain_min, complete) minimize cost;
% solve :: float_search([x[i,j] | i in I, j in J], 0.0001, input_order, indomain_split, complete) minimize cost;


constraint
   % observe supply at source i 
   forall(i in I) (
        sum(j in J) (x[i,j]) = supply[i]
    )

   /\
   % satisfy demand at customer j 
   forall(j in J) (
        sum(i in I) (x[i,j]) = demand[j]
    )

   /\
   % if y[i,j] is 0, force x[i,j] to be 0 (may note that supply[i] and
   % demand[j] are implicit upper bounds for x[i,j] as follows from the
   % constraints f[i] and g[j])
   forall(i in I, j in J) (
      x[i,j] >= 0.0
      /\
      % x[i,j] <= min(supply[i], demand[j]) * int2float(y[i,j])
      x[i,j] <= min(supply[i], demand[j]) * int2float(y[i,j])
   )
;


%
% data;
% 
% These data correspond to the instance bal8x12 from [Balinski]. 

% The optimal solution is 471.55 

m = 8;
n = 12;

supply = [15.00, 20.00, 45.00, 35.00, 25.00, 35.00, 10.00, 25.00];

demand = [20.00, 15.00, 20.00, 15.00, 5.00, 20.00, 30.00, 10.00, 35.00, 25.00, 10.00, 5.00];

varcost = array2d(I,J, 
[0.69,0.64,0.71,0.79,1.70,2.83,2.02,5.64,5.94,5.94,5.94,7.68,
1.01,0.75,0.88,0.59,1.50,2.63,2.26,5.64,5.85,5.62,5.85,4.94,
1.05,1.06,1.08,0.64,1.22,2.37,1.66,5.64,5.91,5.62,5.91,4.94,
1.94,1.50,1.56,1.22,1.98,1.98,1.36,6.99,6.99,6.99,6.99,3.68,
1.61,1.40,1.61,1.33,1.68,2.83,1.54,4.26,4.26,4.26,4.26,2.99,
5.29,5.94,6.08,5.29,5.96,6.77,5.08,0.31,0.21,0.17,0.31,1.53,
5.29,5.94,6.08,5.29,5.96,6.77,5.08,0.55,0.35,0.40,0.19,1.53,
5.29,6.08,6.08,5.29,5.96,6.45,5.08,2.43,2.30,2.33,1.81,2.50]);

fixcost = array2d(I,J,
[11.0,16.0,18.0,17.0,10.0,20.0,17.0,13.0,15.0,12.0,14.0,14.0,
14.0,17.0,17.0,13.0,15.0,13.0,16.0,11.0,20.0,11.0,15.0,10.0,
12.0,13.0,20.0,17.0,13.0,15.0,16.0,13.0,12.0,13.0,10.0,18.0,
16.0,19.0,16.0,11.0,15.0,12.0,18.0,12.0,18.0,13.0,13.0,14.0,
19.0,18.0,15.0,16.0,12.0,14.0,20.0,19.0,11.0,17.0,16.0,18.0,
13.0,20.0,20.0,17.0,15.0,12.0,14.0,11.0,12.0,19.0,15.0,16.0,
11.0,12.0,15.0,10.0,17.0,11.0,11.0,16.0,10.0,18.0,17.0,12.0,
17.0,10.0,20.0,12.0,17.0,20.0,16.0,15.0,10.0,12.0,16.0,18.0]);


output [
"x: " ++ show(x) ++ "\n"++
"y: " ++ show(y) ++ "\n"++
"cost: " ++ show(cost) ++ "\n"
];
