% 
% Transportation problem in MiniZinc.
% 
% From GLPK:s example transp.mod
% 
% """
% A TRANSPORTATION PROBLEM
% 
% This problem finds a least cost shipping schedule that meets
% requirements at markets and supplies at factories.
% 
% References:
%              Dantzig G B, "Linear Programming and Extensions."
%              Princeton University Press, Princeton, New Jersey, 1963,
%              Chapter 3-3.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% canning plants 
int: num_plants;
set of int: I = 1..num_plants;

% markets 
int: num_markets;
set of int: J = 1..num_markets ;

% capacity of plant i in cases 
array[I] of float: a;

% demand at market j in cases 
array[J] of float: b;

% distance in thousands of miles 
array[I,J] of float: d;

% freight in dollars per case per thousand miles 
float: f;

% transport cost in thousands of dollars per case 
% param c{i in I, j in J} := f * d[i,j] / 1000;
array[I,J] of float: c = array2d(I,J, [f*d[i,j] / 1000.0 | i in I, j in J]);


% shipment quantities in cases 
% var x{i in I, j in J} >= 0;
array[I,J] of var 0.0..1000.0: x; % >= 0

% total transportation costs in thousands of dollars 
var float: cost = sum(i in I, j in J) (c[i,j] * x[i,j]);

% solve minimize cost;
solve :: float_search([x[i,j]|i in I, j in J],0.00001,input_order,indomain_split,complete) minimize cost;

constraint
    forall(i in I, j in J) (
       x[i,j] >= 0.0
    )

    /\
    % observe supply limit at plant i 
    forall(i in I) (sum(j in J) (x[i,j]) <= a[i])

    /\
    % satisfy demand at market j 
    forall(j in J) (sum(i in I) (x[i,j]) >= b[j])

;

output 
[ "cost: ", show(cost), "\n",
  "x: ",   
] ++
[
  if j = 1 then "\n" else "\t" endif ++
    show(x[i,j]) 
  | i in I, j in J
]
++ ["\n"] 
;

%
% data
% 
% set I := Seattle San-Diego;
num_plants = 2;

% set J := New-York Chicago Topeka;
num_markets = 3;

a = [350.0, 600.0];
b = [325.0, 300.0, 275.0];

d = array2d(I,J, [ 
  2.5, 1.7, 1.8,
  2.5, 1.8, 1.4
]);

f = 90.0;

