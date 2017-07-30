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

% Since it's implemented as a MIP problem, not all solvers can handle this.
% The only solver I know of that can show all 4 solutions is ECLiPSe/ic.
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%


int: n = 3;
array[1..n] of float: value;
array[1..n] of float: weight;
array[1..n] of float: volume;

array[1..n] of var 0..100000: x;

var float: total_value = sum(i in 1..n) (int2float(x[i])*value[i]);
var float: total_weight = sum(i in 1..n) (int2float(x[i])*weight[i]);
var float: total_volume = sum(i in 1..n) (int2float(x[i])*volume[i]);

solve maximize total_value;
% solve satisfy;

constraint
   total_weight <= 25.0
   /\
   total_volume <= 0.25
   /\
   forall(i in 1..n) ( 
        x[i] >= 0
    ) 

   % for all solutions
   /\ total_value >= 54500.0
;

output [
   "total_value: " ++ show(total_value) ++ "\n" ++
   "total_weight: " ++ show(total_weight) ++ "\n" ++
   "total_volume: " ++ show(total_volume) ++ "\n"
] ++ 
[
  show(i) ++ ": " ++ show(x[i]) ++ "\n"
  | i in 1..n
] 
++ ["\n"];

%
% data
%
value  = [3000.0, 1800.0, 2500.0  ];
weight = [   0.3,    0.2,    2.0  ];
volume = [   0.025,  0.015,  0.002];

