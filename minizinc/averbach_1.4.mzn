% 
% Recreational mathematics in MiniZinc. 
%
% Problem 1.4 from 
% Averbach & Chein "Problem Solving Through Recreational Mathematics".
% """
% Messr Baker, Dyer, Farmer, Glover, and Hosier are seated around a circular table,
% playing poker. Each gentleman is the namesake of the profession of one of the others.
%
% The dyer is seated two places to the left of Mr Hosier.
% The baker sits two places to Mr Baker's right.
% The farmer is seated to the left of Mr Farmer.
% Mr Dyer is on the glover's right.
%
% What is the name of the dyer?
% """

% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% There are two solutions:
% A) 
% [Baker, Dyer, Farmer, Glover, Hosier]: [1, 0, 2, 3, 4]
% [baker, dyer, farmer, glover, hosier]: [3, 2, 1, 4, 0]
% 
% Baker: farmer
% Dyer: hosier
% Farmer: dyer
% Glover: baker
% Hosier: glover
% Sitting positions: Dyer, Baker, Farmer, Glover, Hosier
%
% B)
% [Baker, Dyer, Farmer, Glover, Hosier]: [1, 3, 0, 4, 2]
% [baker, dyer, farmer, glover, hosier]: [3, 0, 4, 2, 1]
%
% Baker: hosier
% Dyer: baker
% Farmer: dyer
% Glover: farmer
% Hosier: glover
% Sitting positions: Farmer, Baker, Hosier, Dyer, Glover
% 
% To answer the original question: 
% In both solutions Mr. Farmer is the dyer.


include "globals.mzn";

% Names
% Numbering the names and professions as 0..4 
% so we easily can use modulo 5.
%
var 0..4: Baker;
var 0..4: Dyer;
var 0..4: Farmer;
var 0..4: Glover;
var 0..4: Hosier;
array[1..5] of var 0..4: names = [Baker, Dyer, Farmer, Glover, Hosier];

% Profession
var 0..4: baker;
var 0..4: dyer;
var 0..4: farmer;
var 0..4: glover;
var 0..4: hosier;
array[1..5] of var 0..4: profession = [baker, dyer, farmer, glover, hosier];


solve satisfy;

constraint
  all_different(names)
  /\
  all_different(profession)
  /\ % Symmetry breaking
  Baker = 1
  /\
  baker  != Baker  /\
  dyer   != Dyer   /\
  farmer != Farmer /\
  glover != Glover /\
  hosier != Hosier
  /\ % The dyer is seated two places to the left of Mr Hosier.
  dyer = (Hosier - 2) mod 5
  /\ % The baker sits two places to Mr Baker's right.
  baker = (Baker + 2) mod 5
  /\ % The farmer is seated to the left of Mr Farmer.
  farmer = (Farmer - 1) mod 5
  /\ % Mr Dyer is on the glover's right.
  Dyer = (glover + 1) mod 5
;


output [
 "[Baker, Dyer, Farmer, Glover, Hosier]: ", show([Baker, Dyer, Farmer, Glover, Hosier]), "\n",
 "[baker, dyer, farmer, glover, hosier]: ", show([baker, dyer, farmer, glover, hosier]), "\n"

];
