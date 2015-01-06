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

% decision variables
var 1..n: arthur;
var 1..n: bella;
var 1..n: craig;
var 1..n: duncan;
var 1..n: elizabeth;
var 1..n: francesca;
array[1..n] of string: x_s = 
[
"arthur   ",
"bella    ",
"craig    ",
"duncan   ",
"elizabeth",
"francesca"
];

array[1..n] of var 1..n: x = [arthur,bella,craig,duncan,elizabeth,francesca];

predicate above(var int: a, var int: b) =
   (a = 4 /\ b = 1) \/
   (a = 5 /\ b = 2) \/
   (a = 6 /\ b = 3)
;

predicate adjacent(var int: a, var int: b) =
   {a,b} = {4,5} \/
   {a,b} = {5,6} \/
   {a,b} = {1,2} \/
   {a,b} = {2,3}
;

function var int: room_number(var int: r) =
   (r-1) mod 3
;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  all_different(x) 

  %  - Arthur is directly above Bella, who works next to Duncan.
  /\ above(arthur,bella)
  /\ adjacent(bella,duncan)

  %  - Elizabeth’s office number is smaller than Francesca’s.

  /\ room_number(elizabeth) < room_number(francesca)

  %  - Craig and Francesca are in adjacent rooms.
  /\ adjacent(craig,francesca)

;

output [
  show(x), "\n",
]
++
[
  show(join(" ",[x_s[p] | r in 4..6, p in 1..n where fix(x[p]) = r])), "\n",
  show(join(" ", [x_s[p] | r in 1..3, p in 1..n where fix(x[p]) = r])), "\n",
]
;

