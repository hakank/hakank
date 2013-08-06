% 
% Enigma puzzle 1293 in MiniZinc.
% 
% From ECLiPSe example enigma1293.ecl:
% """
% Enigma 1293 - Reverse Fahrenheit
% New Scientist magazine, 12 June 2004.
% by Michael slater.
% 
% "Multiplying by 9/5 and adding 32," I explained to my clever
% nephew George, "is useless in practice.  What you need is some
% memorable equivalents, like 10 ^(o)C being 50 Fahrenheit.  Here's
% one I've invented: 16 ^(o)C = 61 ^(o)F.  See, to get from one to the
% other you just reverse the two digits."
% 
% "Actually 16 ^(o)C = 60.8 ^(o)F," I said.
% 
% "So 61 is near enough," I said.
% 
% "Near enough is not exactly right."
% 
% "But you cannot do it exactly," I objected sourly.
% 
% "You can't, because you insist on boring old base 10.  But I bet
% I can, using other bases," George retorted.  Off he went to
% investigate, and was soon back.  "-90 ^(o)C = -130 ^(o)F," he said,
% "and to base 21 this says -46 ^(o)C = -64 ^(o)F.  I have other examples,
% including two between the freezing and boiling points of water."
% 
% What were the two examples that George found?  Give your answers
% in the form x ^(o)C = y ^(o)F where x and y are written in base 10
% (and x lies between 0 and 100).
% """

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 


var 0..100: C;
var 2..100: Base;
var int: D1;
var int: D2;
var int: F;

solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint
  5*F = 9*C + 160 /\		% standard C->F conversion rule
  D1 >= 0 /\ D1 < Base /\	% and two Base-digits D1 and D2
  D2 >= 0 /\ D2 < Base /\
  C = Base*D1 + D2 /\		% C and F in terms of D1 and D2
  F = Base*D2 + D1
;

output
[
  "C: " ++ show(C) ++ "\n" ++
  "F: " ++ show(F) ++ "\n" ++
  "Base: " ++ show(Base) ++ "\n" ++
  "D1: " ++ show(D1) ++ "\n" ++
  "D2: " ++ show(D2) ++ "\n"
];
