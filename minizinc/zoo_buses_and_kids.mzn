% 
% Zoo, Buses, and Kids problem in MiniZinc.
% 
% https://dmcommunity.org/challenge/challenge-july-2018/
%"""
% 300 kids need to travel to the London zoo. The school may rent 40 seats and 
% 30 seats buses for 500 and 400 £. How many buses of each to minimize cost?
% """
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: num_kids = 300;
array[1..2] of int: seats = [30,40];
array[1..2] of int: price = [400,500];

% decision variables
array[1..2] of var 0..100: buses; % number of buses of each size
var int: z = sum([buses[i]*price[i] | i in 1..2]);

solve minimize z;
% solve satisfy;

constraint
  sum([seats[i]*buses[i] | i in 1..2]) >= num_kids
;

output [
  "\(buses)\nz:\(z)\n"
];

