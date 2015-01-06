% 
% Office Blocked problem in MiniZinc.
% 
% From 
% http://l4f.cecs.anu.edu.au/puzzles/beginner/office-blocked
% """
% Six hardworking employees of the Paper Circulation Division of the 
% Department of Miscellania occupy offices on two floors of the 
% Administration Building: three on the upper floor (offices U1, U2 and U3) 
% and the other three directly below them on the lower floor (L1, L2 and L3):
%
% [
% 
%    floor                     representation
%       2    U1   U2   U3       4  5  6
%       1    L1   L2   L3       1  2  3
%
% ]
% 
%  - Arthur is directly above Bella, who works next to Duncan.
%  - Elizabeth’s office number is smaller than Francesca’s.
%  - Craig and Francesca are in adjacent rooms.
% 
% Who works where?
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 


int: floors = 2;
int: rooms = 3;
int: n = floors*rooms;

int: arthur = 1;
int: bella  = 2;
int: craig  = 3;
int: duncan = 4;
int: elizabeth = 5;
int: francesca = 6;

array[1..n] of string: x_s = 
[
"arthur   ",
"bella    ",
"craig    ",
"duncan   ",
"elizabeth",
"francesca"
];


% decision variables
array[1..n] of var 1..floors: floor;
array[1..n] of var 1..rooms: room;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint

  % ensure different rooms
  all_different([(floor[i]-1)*rooms + room[i] | i in 1..n])

  %  - Arthur is directly above Bella, who works next to Duncan.
  /\ floor[arthur] = 2 /\ floor[bella] = 1 /\ room[arthur] = room[bella]
  /\ floor[bella] = floor[duncan] /\ abs(room[bella]-room[duncan]) = 1

  %  - Elizabeth’s office number is smaller than Francesca’s.
  /\ room[elizabeth] < room[francesca]

  %  - Craig and Francesca are in adjacent rooms.
  /\ floor[craig] = floor[francesca] /\ abs(room[craig]-room[francesca]) = 1

;

output [
  "floor: ", show(floor), "\n",
  "room : ", show(room), "\n",
  "     : ", show([(floor[i]-1)*rooms + room[i] | i in 1..n]), "\n"
]
++
[
  if r = 1 /\ p = 1 then "\n" else "" endif ++ 
  if fix(floor[p]) = 3-f /\ fix(room[p]) = r then
     x_s[p] ++ " "
  else
    ""
  endif
  | f in 1..floors, r in 1..rooms, p in 1..n
]
;

