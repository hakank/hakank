% 
% Product Configuration in MiniZinc
% 
% From  
% John Hooker, 
% A framework for integrating optimization and constraint programming, 
% http://web.tepper.cmu.edu/jnh/sara07.pdf
% page 47ff:
% """
% Choose what type of each component, and how many.
%
% Component       Type  Net power   Disk    Memory    Weight    Max number
%                      generation  space   capacity             used
%   i               k     A1jk      A2jk     A3jk      A4jk
% 1. Power supply   A     70          0       0        200        1
%                   B    100          0       0        250
%                   C    150          0       0        350
% 2. Disk drive     A    -30        500       0        140        3
%                   B    -50        800       0        300
% 3. Memory chip    A    -20          0     250         20        3
%                   B    -25          0     300         25
%                   C    -30          0     400         30
% Lower bound L(j)         0        700     850          0
% Upper bound U(j)         -          -       -          -
% Unit cost(j)             0          0       0          1
%
% """
% 
% The solution according to page 66 is:
%  1 of Power supply C
%  2 of Disk drive A
%  3 of Memory B
%
% 
% which in this model translates to 
% x=  [0,0,1,  2,0, 0,3,0 ]
% with a cost (z) of 705
% 
% However, this model gives another solution:
%
% x = [0,0,1,  2,0, 0,0,2]
% z = 690
% That is, instead of 3 Memory B we have 2 of Memory C.

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

int: c;              % number of component types 
int: t;              % number of configuration types
int: n;              % total number of components

% note:
% add 1 to t (types) since first index is the component type (1..c)

% component, type
array[1..n, 1..t+1] of int: A; % the data matrix
array[1..c] of int: max_number_used; % num components to used
array[1..c] of int: min_number_used; % num components to used

array[1..t+1] of int: lower_bound;
% array[1..t+1] of int: upper_bound;
array[1..t+1] of int: unit_cost;

% decision variables
array[1..n] of var int: x;

% Hooker's optimization:
% The total weight should be minimized.
% Hooker's goal is per unit, but float things in Minizinc (as of 
%  version ~0.7) don't work (for minizinc and fz, or is really slow in eclipse).
% var float: total_weight = sum(i in 1..n, j in 1..t+1) (
%         int2float(x[i]*A[i,j]*unit_cost[j]) / int2float(x[i])
% );
%
% int version: total_weight
var int: total_weight = sum(i in 1..n, j in 1..t+1) (
        x[i]*A[i,j]*unit_cost[j]
);


%
% What if we want the hottest machine?
% var int: z = sum(i in 1..n, j in 2..4) (
%        x[i]*A[i,j]
% );



% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) minimize total_weight;
% solve maximize z;

constraint
   %   total_weight <= 690 % for solve constraint
   % x = [0,0,1,  2,0, 0,3,0 ] % Hooker's solution
   % /\
   forall(i in 1..n) (
     x[i] >= 0
   )   
   /\ % bound: max and min number used of each component type
   forall(j in 1..c) (
     sum(i in 1..n where A[i,1] = j) ( 
        x[i]
      ) <= max_number_used[j]
     /\
     sum(i in 1..n where A[i,1] = j) ( 
        x[i]
      ) >= min_number_used[j]

   )
   /\ % lower bound of each configuration type 
   forall(j in 1..t+1) (
     sum(i in 1..n) (
        x[i]*A[i,j]) >= lower_bound[j]
   )
;


%
% data
%
c = 3;
t = 4;
n = 8;
max_number_used = [1,3,3]; % max numb components of each type (1..c)
min_number_used = [1,1,1]; % min numb components of each type (1..c)
unit_cost =   [0, 0,   0,   0, 1]; % minimize the weight
lower_bound = [0, 0, 700, 800, 0];


%
% type, net power gen,  disk space, memory capacity, weight
%

A = 
array2d(1..n, 1..t+1, [
 % power supply, generating power
 1,   70,          0,       0,        200,
 1,  100,          0,       0,        250,
 1,  150,          0,       0,        350,

 % disk drive, consuming power
 2,  -30,        500,       0,        140,
 2,  -50,        800,       0,        300,

 % memory_chip,consuming power
 3,   -20,          0,     250,         20,
 3,   -25,          0,     300,         25,
 3,   -30,          0,     400,         30
]);


output
[
  "x: " ++ show(x) ++ "\n" ++
  "total_weight: " ++ show(total_weight) ++ "\n" 
];