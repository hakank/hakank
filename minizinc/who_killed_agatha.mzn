% 
% Who killed agatha? (The Dreadsbury Mansion Murder Mystery) in MiniZinc.
%
% This is a standard benchmark for theorem proving.
%
% http://www.lsv.ens-cachan.fr/~goubault/H1.dist/H1.1/Doc/h1003.html
% 
% """ 
% Someone in Dreadsbury Mansion killed Aunt Agatha. 
% Agatha, the butler, and Charles live in Dreadsbury Mansion, and 
% are the only ones to live there. A killer always hates, and is no 
% richer than his victim. Charles hates noone that Agatha hates. Agatha 
% hates everybody except the butler. The butler hates everyone not richer 
% than Aunt Agatha. The butler hates everyone whom Agatha hates. 
% Noone hates everyone. Who killed Agatha? 
% """

% Originally from 
% F. J. Pelletier: Seventy-five problems for testing automatic theorem provers. Journal of Automated Reasoning, 2: 191–216, 1986.
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% 8 solutions which all gives the same result:
% Agatha killed herself.

include "globals.mzn";

int: n = 3;
set of int: r = 1..3;

% persons
r: agatha  = 1;
r: butler  = 2;
r: charles = 3;

var r: the_killer;
var r: the_victim;

array[r,r] of var 0..1: hates;
array[r,r] of var 0..1: richer;

solve satisfy;

constraint

   % Agatha, the butler, and Charles live in Dreadsbury Mansion, and 
   % are the only ones to live there. 

   % A killer always hates, and is no richer than his victim. 
   hates[the_killer, the_victim] = 1
   /\
   richer[the_killer, the_victim] = 0

   /\ % define the concept of richer: no one is richer than him-/herself
   forall(i in r) (
      richer[i,i] = 0
   )

   /\ % (contd...) if i is richer than j then j is not richer than i
   forall(i, j in r where i != j) (
      richer[i,j] = 1 <-> richer[j,i] = 0
   )

   % Charles hates noone that Agatha hates. 
   /\ 
   forall(i in r) (
      hates[charles, i] = 0 <- hates[agatha, i] = 1
   )

   % Agatha hates everybody except the butler. 
   /\ 
   hates[agatha, charles] = 1 
   /\ 
   hates[agatha, agatha] = 1
   /\
   hates[agatha, butler] = 0

   % The butler hates everyone not richer than Aunt Agatha. 
   /\ 
   forall(i in r) (
     hates[butler, i] = 1 <- richer[i, agatha] = 0
   )

   % The butler hates everyone whom Agatha hates. 
   /\ 
   forall(i in r) (
      hates[butler, i] = 1 <- hates[agatha, i] = 1
   )

   % Noone hates everyone. 
   /\
   forall(i in r) (
     sum(j in r) (hates[i,j]) <= 2
     
   )

   % Who killed Agatha? 
   /\ the_victim = agatha

;


output [
  "the killer: ", show(the_killer), "\n",
  "the victim: ", show(the_victim),
] ++ 
["\nhates:"] ++
[
  if j = 1 then "\n" else " " endif ++
    show(hates[i,j])
  | i, j in r
] ++ ["\nricher:"] ++
[
  if j = 1 then "\n" else " " endif ++
    show(richer[i,j])
  | i, j in r
] ++
[ "\n" ]
++
[
  "hates: " ++ show(hates) ++ "\n" ++
  "richer: " ++ show(richer) ++ "\n"
]
;

