% 
% Mrs Timpkin's Age problem in MiniZinc.
% 
% From 
% http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/index.html
% """
% Mrs Timpkin's Age    from "Amusements in Mathematics, Dudeney", number 43.
%
% When the Timpkinses married eighteen years ago, Timpkins was three
% times as old as his wife, and today he is just twice as old as she.
% How old is Mrs. Timpkin? 
% """
%
% Compare with my other models:
% * ECLiPSe : http://www.hakank.org/eclipse/timpkin.ecl
% * SICStus Prolog: http://www.hakank.org/sicstus/timpkin.pl
% * Gecode: http://www.hakank.org/gecode/timpkin.cpp

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

set of int: ages = 1..100;
var ages: T;
var ages: W;
        
solve satisfy;
constraint

  T - 18 = 3 * (W - 18) /\
  T = 2 * W
;


output [show([T,W]) ];