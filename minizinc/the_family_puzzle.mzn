% 
% The Familiy Puzzle in MiniZinc.
% 
%   
% From Drools Puzzle Round 2: The Familiy Puzzle
% http://blog.athico.com/2007/08/drools-puzzle-round-2-familiy-puzzle.html
% """
% 
% * Three men, Abel, Locker and Snyder are married to Edith, Doris and Luisa, 
%   but not necessarily in this order.
% * Each couple has one son.
% * The sons are called Albert, Henry and Victor.
% * Snyder is nor married to Luisa, neither is he Henry's father.
% * Edit is not married to Locker and not Albert's mother.
% * If Alberts father is either Locker or Snyder, then Luisa is Victor's mother.
% * If Luisa is married to Locker, then Doris is not Albert's mother. 
% 
% Who is married to whom and what are their sons called?
%
% Taken from the German book "Denken als Spiel" by Willy Hochkeppel, 1973 
% (Thinking as a Game).  
% """
% 
% Solutions and discussions
% http://ningning.org/blog2/2008/05/25/drools-puzzles-result-round-2-the-familiy-puzzle
% http://rbs.gernotstarke.de/samples/page21/page21.html
%
% Compare with the Comet model:
% http://www.hakank.org/comet/the_family_puzzle.co
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn"; 
int: n = 3;
set of int: R = 1..n;

int: Abel = 1;
int: Locker = 2;
int: Snyder = 3;
array[R] of int: men = [Abel, Locker, Snyder];

% decision variables
var R: Edith;
var R: Doris;
var R: Luisa;
array[R] of var int: women  = [Edith, Doris, Luisa];

var R: Albert;
var R: Henry;
var R: Victor;
array[R] of var int: sons = [Albert, Henry, Victor];

solve satisfy;

constraint
  all_different(women) /\
  all_different(sons) /\

  % Snyder is nor married to Luisa, neither is he Henry's father.
  Snyder != Luisa /\
  Snyder != Henry /\

  % Edith is not married to Locker and not Albert's mother.
  Edith != Locker /\
  Edith != Albert /\

  % If Alberts father is either Locker or Snyder, 
  % then Luisa is Victor's mother.
  (
         (Albert = Locker \/ Albert = Snyder) -> 
         Luisa = Victor
  )
  /\

  % If Luisa is married to Locker, 
  % then Doris is not Albert's mother. 
  (
      Luisa = Locker -> 
      Doris != Albert
  )
;

output [
"men: " ++ show(men) ++ "\n"++
"women: " ++ show(women) ++ "\n"++
"sons: " ++ show(sons) ++ "\n"
];
