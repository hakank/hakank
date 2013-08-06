% 
% Car sequencing in MiniZinc.
% 
% This is based on the OPL3 model car.mod.
%
% Compare with the Comet model
% http://www.hakank.org/comet/car.co
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: nbCars;
int: nbOptions;
int: nbSlots;
set of int: Cars = 1..nbCars;
set of int: Options = 1..nbOptions;
set of int: Slots = 1..nbSlots;

array[Cars] of int: demand;

array[Options,Cars] of int: option;
array[Options, 1..2] of int: capacity;

array[Options] of int: optionDemand = [sum(j in Cars) (demand[j] * option[i,j]) | i in Options];

% decision variables

array[Slots] of var Cars: slot;
array[Options, Slots] of var 0..1: setup;


var int: z = sum(s in Cars) (s*slot[s]);

solve minimize z;
% solve satisfy;

constraint
  forall(c in Cars ) (
    sum(s in Slots ) (bool2int(slot[s] = c)) = demand[c]
  )
  /\
  forall(o in Options, s in 1..nbSlots - capacity[o,2] + 1) (
    sum(j in s..s + capacity[o,2]- 1) (setup[o,j]) <= capacity[o,1]
  )
  /\
  forall(o in Options, s in Slots ) (
    setup[o,s] = option[o,slot[s]]
  )
  /\
  forall(o in Options, i in 1..optionDemand[o]) (
    sum(s in 1..(nbSlots - i * capacity[o,2])) (setup[o,s]) >=
           (optionDemand[o] - i * capacity[o,1])
  )
;

% for solve satisfy
% constraint z = 82;


output [
 "z: " ++ show(z) ++ "\n" ++
 "slot: " ++ show(slot) ++ "\n"
] ++
[
  if j = 1 then "\n" else " " endif ++ 
    show(setup[i,j])
  | i in Options, j in Slots
];


%
% data
%

% From OPL, car dat (smallest instance)
nbCars = 6;
nbOptions = 5;
nbSlots = 10;
demand = [1, 1, 2, 2, 2, 2];
option =
  array2d(Options,Cars, 
    [
        1, 0, 0, 0, 1, 1,
        0, 0, 1, 1, 0, 1,
        1, 0, 0, 0, 1, 0,
        1, 1, 0, 1, 0, 0,
        0, 0, 1, 0, 0, 0
    ]);
capacity = 
  array2d(Options, 1..2,
   [
        1,2,
        2,3,
        1,3,
        2,5,
        1,5
    ]);
