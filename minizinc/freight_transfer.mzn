% 
% Freight Transfer in MiniZinc
% 
% From  
% John Hooker, 
% A framework for integrating optimization and constraint programming, 
% http://web.tepper.cmu.edu/jnh/sara07.pdf
% page 23f
% """
% Transport 42 tons of freight using 8 trucks, which come in
% 4 sizes...
% 
% Truck Number Capacity  Cost
%  type available (tons)  per
%                        truck
%   1      3         7     90
%   2      3         5     60
%   3      3         4     50
%   4      3         3     40
% """
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: n;                        % number of trucks
array[1..n] of int: available; % number trucks available
array[1..n] of int: capacity;  % capacity of truck
array[1..n] of int: cost;      % cost per truck
int: transport;                % ton to transport

% vars
array[1..n] of var 0..n: x;    % number of trucks to use of each size
var int: z = sum(i in 1..n) (x[i]*cost[i]);
var int: transported = sum(i in 1..n) ( x[i]*capacity[i] );


% solve satisfy;
solve minimize z;

constraint
%   z <= 530
%  /\
  transported >= transport
  /\
  forall(i in 1..n) (
     x[i] <= available[i]
  )
;


%
% data
%
n = 4;
available = [3,3,3,3];
capacity = [7,5,4,3];
cost = [90,60,50,40];
transport = 42;

output [
  "z: ", show(z), "\n",
  "x: ", show(x), "\n",
  "transported: ", show(transported), "\n",
];