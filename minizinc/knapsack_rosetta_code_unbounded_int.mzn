% 
% Knapsack (Unbounded) in MiniZinc.
% 
% http://rosettacode.org/wiki/Knapsack_problem/Unbounded
% """
% A traveller gets diverted and has to make an unscheduled stop in what 
% turns out to be Shangri La. Opting to leave, he is allowed to take as much 
% as he likes of the following items, so long as it will fit in his knapsack, 
% and he can carry it. He knows that he can carry no more than 25 'weights' in 
% total; and that the capacity of his knapsack is 0.25 'cubic lengths'.
% 
% Looking just above the bar codes on the items he finds their weights and 
% volumes. He digs out his recent copy of a financial paper and gets the 
% value of each item.
%
% Item	                 Explanation	                Value (each)   weight	Volume (each)
% --------------------------------------------------------------------------------------------
% panacea (vials of)	Incredible healing properties	3000           0.3	0.025
% ichor (ampules of)	Vampires blood	                1800           0.2	0.015
% gold (bars)	        Shiney shiney	                2500           2.0	0.002
% Knapsack	        For the carrying of	        -              <= 25	<=0.25 
%
% He can only take whole units of any item, but there is much more of 
% any item than he could ever carry
%
% How many of each item does he take to maximise the value of items 
% he is carrying away with him?
%
% Note: There are four solutions that maximise the value taken. Only one need be given. 
% """

% This variant of http://www.hakank.org/minizinc/knapsack_rosetta_code2.mzn
% use finite domains (i.e. integers) instead of floats.

% 
% All 4 optimal solutions:
%   total_value: 54500 (54500.0)
%   total_weight: 250 (25.0)
%   total_volume: 247 (0.247)
%   1: 0
%   2: 15
%   3: 11
%
%   ----------
%   total_value: 54500 (54500.0)
%   total_weight: 249 (24.9)
%   total_volume: 247 (0.247)
%   1: 3
%   2: 10
%   3: 11
% 
%   ----------
%   total_value: 54500 (54500.0)
%   total_weight: 248 (24.8)
%   total_volume: 247 (0.247)
%   1: 6
%   2: 5
%   3: 11
% 
%   ----------
%   total_value: 54500 (54500.0)
%   total_weight: 247 (24.7)
%   total_volume: 247 (0.247)
%   1: 9
%   2: 0
%   3: 11


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%


int: n = 3;
array[1..n] of int: value;
array[1..n] of int: weight;
array[1..n] of int: volume;

array[1..n] of var int: x;

var int: total_value = sum(i in 1..n) (x[i]*value[i]);
var int: total_weight = sum(i in 1..n) (x[i]*weight[i]);
var int: total_volume = sum(i in 1..n) (x[i]*volume[i]);

solve :: int_search(x, first_fail, indomain_max, complete) maximize total_value;
% solve satisfy;

constraint
   total_weight <= 250 % multiply with 10
   /\
   total_volume <= 250 % multiply with 1000
   /\
   forall(i in 1..n) ( 
        x[i] >= 0
    ) 

   % for all solutions (solve satisfy)
   % /\ total_value >= 54500
;

output [
   "total_value : " ++ show(total_value)  ++ " (" ++ show(int2float(total_value)/1.0) ++ ")\n" ++
   "total_weight: " ++ show(total_weight) ++ " (" ++ show(int2float(total_weight)/10.0) ++ ")\n" ++
   "total_volume: " ++ show(total_volume) ++ " (" ++ show(int2float(total_volume)/1000.0) ++ ")\n"
] ++ 
[
  show(i) ++ ": " ++ show(x[i]) ++ "\n"
  | i in 1..n
] 
++ ["\n"];

%
% data
%
% integer values
% 
value  = [3000, 1800, 2500 ];  % 
weight = [   3,    2,   20 ];  % muliply with 10
volume = [   25,  15,    2 ];  % multiply with 1000

