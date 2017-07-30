% 
% Knapsack problem in MiniZinc.
% 
% From
% http://www.rosettacode.org/wiki/Knapsack_Problem
% """
% A traveller gets diverted and has to make an unscheduled stop in what turns out to 
% be Shangri La. Opting to leave, he is allowed to take as much as he likes of the 
% following items, so long as it will fit in his knapsack, and he can carry it. He knows 
% that he can carry no more than 25 'weights' in total; and that the capacity of his 
% knapsack is 0.25 'cubic lengths'.
% 
% Looking just above the bar codes on the items he finds their weights and volumes. 
% He digs out his recent copy of a financial paper and gets the value of each item.
% 
% Item	Explanation	                               Value (each) weight  Volume (each)
% ------------------------------------------------------------
% panacea (vials of)	Incredible healing properties	3000	0.3	0.025
% ichor (ampules of)	Vampires blood	                1800	0.2	0.015
% gold (bars)	        Shiney shiney	                2500	2.0	0.002
% ----------------------------------------------------------------------------
% Knapsack	        For the carrying of	-                <=25	<=0.25 
% 
% He can only take whole units of any item,, but there is much more of any item than 
% he could ever carry
% 
% How many of each item does he take to maximise the value of items he is carrying 
% away with him?
%
% Note:
%
%    1. There are four solutions that maximise the value taken. Only one need be given. 
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: n = 3;
array[1..n] of float: value;
array[1..n] of float: weight;
array[1..n] of float: volume;

array[1..n] of var 0..10000: x;

var float: z = sum(i in 1..n) (int2float(x[i])*value[i]);

% 
% knapsack integer version
% 
predicate knapsack(array[int] of var int: w, 
                   array[int] of var 0..1: take, 
                   var int: wtmax) =
           sum(i in index_set(w)) (w[i] * take[i]) <= wtmax
;

% 
% knapsack float version
% 
predicate knapsack(array[int] of var float: w, 
                   array[int] of var 0..1: take, 
                   var float: wtmax) =
   sum(i in index_set(w)) (w[i] * int2float(take[i])) <= wtmax
;


solve maximize z;
% solve :: int_search(x, input_order, indomain_split, complete) maximize z;

constraint
   z >= 0.0 /\
   forall(i in 1..n) ( x[i] >= 0 ) 
   /\
   knapsack(volume, x, 0.25) 
   /\
   knapsack(weight, x, 25.0)
;


%
% data
%
value  = [3000.0, 1800.0, 2500.0  ];
weight = [   0.3,    0.2,    2.0  ];
volume = [   0.025,  0.015,  0.002];


output [
"x: " ++ show(x) ++ "\n"++
"z: " ++ show(z) ++ "\n"
];
