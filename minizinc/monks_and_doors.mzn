% 
% Monks and doors problem in MiniZinc.
% 
% From http://user.it.uu.se/~rolandb/LP/gammal/960615_facit.ps
% """
% There is a room with four doors and eight monks. One or more of
% the doors may be exit. Each monk is either telling a lie or the truth.
% 
% The monks make the following statements:
% Monk 1: Door A is the exit.
% Monk 2: At least one of the doors B and C is the exit.
% Monk 3: Monk 1 and Monk 2 are telling the truth.
% Monk 4: Doors A and B are both exits.
% Monk 5: Doors A and B are both exits.
% Monk 6: Either Monk 4 or Monk 5 is telling the truth.
% Monk 7: If Monk 3 is telling the truth, so is Monk 6.
% Monk 8: If Monk 7 and Monk 8 are telling the truth, so is Monk 1.
% 
% Which door is an exit no matter who is a liar and who is telling the
% truth.
% """
% 
% Answer: Door A is an exit.
%         And monks 1, 7, and 8 are telling the truth.

% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

var bool: A;
var bool: B;
var bool: C;
var bool: D;

var bool: M1;
var bool: M2;
var bool: M3;
var bool: M4;
var bool: M5;
var bool: M6;
var bool: M7;
var bool: M8;

solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint

% Monk 1: Door A is the exit.
   (M1 <-> A)
% Monk 2: At least one of the doors B and C is the exit.
  /\
  (M2 <-> (bool2int(B) + bool2int(C)) >= 1)
% Monk 3: Monk 1 and Monk 2 are telling the truth.
  /\
  (M3 <-> M1 /\ M2) 
% Monk 4: Doors A and B are both exits.
  /\
  (M4 <-> (A /\ B))
% Monk 5: Doors A and C are both exits.
  /\
  (M5 <-> (A /\ C))
% Monk 6: Either Monk 4 or Monk 5 is telling the truth.
  /\
  (M6 <-> (M4 \/ M5))
% Monk 7: If Monk 3 is telling the truth, so is Monk 6.
  /\
  (M7 <-> (M3 -> M6))
% Monk 8: If Monk 7 and Monk 8 are telling the truth, so is Monk 1.
  /\
  (M8 <-> ((M7 /\ M8) -> M1))
  
% Exactly one door is an exit.
  /\ 
  (bool2int(A) + bool2int(B) + bool2int(C) + bool2int(D) = 1)
;


output [
  "doors: ", show([A,B,C,D]), "\n",
  "monks: ", show([M1,M2,M3,M4,M5,M6,M7,M8]), "\n",
]
;
