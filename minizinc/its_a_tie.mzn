% 
% "It's a tie" problem (Dell Logic Puzzles, October 1999) in MiniZinc.
% 
% From 
% http://socrates.bmcc.cuny.edu/jsamuels/text/mhh-discrete-03ext.pdf
% """
% Four colleagues recently got into a discussion about some of the 
% flamboyant patterns showing up on neckties these days. As a 
% joke, each man arrived at work the next day sporting the most
% ridiculous tie he could find (no two men wore ties with the same 
% pattern—one tie was decorated with smiling cupids). None of the 
% men had to venture outside of his own closet, as each had received 
% at least one such tie from a different relative! From the 
% following clues, can you match each man with the pattern on 
% his flamboyant tie, as well as determine the relative who 
% presented each man with his tie?
% (a) The tie with the grinning leprechauns wasn't a
%     present from a daughter.
% (b) Mr. Crow’s tie features neither the dancing reindeer 
%     nor the yellow happy faces.
% (c) Mr. Speigler’s tie wasn’t a present from his uncle.
% (d) The tie with the yellow happy faces wasn’t a gift
%     from a sister.
% (e) Mr. Evans and Mr. Speigler own the tie with the
%     grinning leprechauns and the tie that was a present
%     from a father-in-law, in some order.
% (f) Mr. Hurley received his flamboyant tie from his sister.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 4;

int: crow = 1;
int: evans = 2;
int: hurley = 3;
int: speigler = 4;
array[1..n] of int: mr = [crow, evans, hurley, speigler];
array[1..n] of string: mr_s = ["Crow", "Evans", "Hurley", "Speigler"];

var 1..n: cupids;
var 1..n: happy_faces;
var 1..n: leprechauns;
var 1..n: reindeer;
array[1..n] of var 1..n: ties = [cupids, happy_faces, leprechauns, reindeer];
array[1..n] of string: ties_s = ["cupids", "happy_faces", "leprechauns", "reindeer"];
array[1..n] of var 1..n: ties_inv;

var 1..n: daughter;
var 1..n: father_in_law;
var 1..n: sister;
var 1..n: uncle;
array[1..n] of var 1..n: relative = [daughter, father_in_law, sister, uncle];
array[1..n] of string: relative_s = ["daughter", "father-in-law", "sister", "uncle"];
array[1..n] of var 1..n: relative_inv;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  alldifferent(ties) /\
  alldifferent(relative) /\

  inverse(ties, ties_inv) /\
  inverse(relative, relative_inv) /\

  % 1. The tie with the grinning leprechauns wasn't a present from a daughter.
  leprechauns != daughter /\

  % 2. Mr. Crow's tie features neither the dancing reindeer nor the yellow happy faces.
  crow != reindeer /\
  crow != happy_faces /\

  % 3. Mr. Speigler's tie wasn't a present from his uncle.
  speigler != uncle /\

  % 4. The tie with the yellow happy faces wasn't a gift from a sister.
  happy_faces != sister /\

  % 5. Mr Evans and Mr. Speigler own the tie with the grinning leprechauns
  %    and the tie that was a present from a father-in-law, in some order.
  (
    (evans = leprechauns /\ speigler = father_in_law)
    \/
    (speigler = leprechauns /\ evans = father_in_law)
  )
  /\
  % 6. Mr. Hurley received his flamboyant tie from his sister.
  hurley = sister

;

output [
  "mr            : " ++ show(mr) ++ "\n" ++
  "mr_s          : " ++ show(mr_s) ++ "\n" ++
  "ties          : " ++ show(ties) ++ "\n" ++
  "ties_inv      : " ++ show(ties_inv) ++ "\n" ++
  "ties_inv_s    : " ++ show([ties_s[fix(ties_inv[i])] | i in 1..n]) ++ "\n" ++
  "relative      : " ++ show(relative) ++ "\n" ++
  "relative_inv  : " ++ show(relative_inv) ++ "\n" ++
  "relative_inv_s: " ++ show([relative_s[fix(relative_inv[i])] | i in 1..n]) ++ "\n\n"
] ++
[
  "Mr. " ++ show(mr_s[i]) ++ " got the " ++ show(ties_s[fix(ties_inv[i])]) ++ " from his " ++ show(relative_s[fix(relative_inv[i])]) ++ ".\n"
  | i in 1..n
]
++
["\n"]
;

