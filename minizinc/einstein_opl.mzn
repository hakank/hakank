% 
% Einstein puzzle in MiniZinc.
% 
% This is a Minizinc version of the OPL code presented in 
% Daniel Selman's "Einstein's Puzzle - Or, 'The Right Tool for the Job'"
% http://blogs.ilog.com/brms/2009/02/16/einsteins-puzzle/
% """
% * Norwegian cats water Dunhill yellow
% * Dane horses tea Blends blue
% * Brit birds milk Pall Mall red
% * German fish coffee Prince green
% * Sweed dogs beer Blue Master white
% """
% 
% I let the "Sweed" stand, it should - of course - be "Swede".
%
% See 
%  * http://www.stanford.edu/~laurik/fsmbook/examples/Einstein%27sPuzzle.html
%
% Compare with the Comet model
% http://www.hakank.org/comet/einstein_opl.co
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn"; 

int: Brit = 1;
int: Sweed = 2;
int: Dane = 3;
int: Norwegian = 4;
int: German = 5;
set of int: States = 1..5;

int: dogs = 1;
int: fish = 2;
int: birds = 3;
int: cats = 4;
int: horses = 5;
set of int: Animals = 1..5;

int: tea = 1;
int: coffee = 2;
int: milk = 3;
int: beer = 4;
int: water = 5;
set of int: Drinks = 1..5;

int: Pall_Mall = 1;
int: Dunhill = 2;
int: Blends = 3;
int: Blue_Master = 4;
int: Prince = 5;
set of int: Smoke = 1..5;

int: red = 1;
int: green = 2;
int: white = 3;
int: yellow = 4;
int: blue = 5;
set of int: Color = 1..5;


% decision variables
array[States] of var States: s;
array[Animals] of var Animals: a;
array[Drinks] of var Drinks: d;
array[Smoke] of var Smoke: k;
array[Color] of var Color: c;


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint

  % allDifferent needed
  alldifferent(s) /\
  alldifferent(a) /\
  alldifferent(d) /\
  alldifferent(k) /\
  alldifferent(c) /\

  % 1. The Brit lives in a red house.
  s[Brit] = c[red] /\
  % 2. The Swede keeps dogs as pets.
  s[Sweed] = a[dogs] /\
  % 3. The Dane drinks tea.
  s[Dane] = d[tea] /\
  % 5. The owner of the Green house drinks coffee.
  c[green] = d[coffee] /\
  % 6. The person who smokes Pall Mall rears birds.
  k[Pall_Mall] = a[birds] /\
  % 7. The owner of the Yellow house smokes Dunhill.
  c[yellow] = k[Dunhill] /\
  % 8. The man living in the centre house drinks milk.
  d[milk] = 3 /\
  % 9. The Norwegian lives in the first house.
  s[Norwegian] = 1 /\
  % 12. The man who smokes Blue Master drinks beer.
  k[Blue_Master] = d[beer] /\
  % 13. The German smokes Prince.
  s[German] = k[Prince] /\
  % 4. The Green house is on the left of the White house.
  c[green] = c[white]-1 /\
  % 10. The man who smokes Blends lives next to the one who keeps cats.
  abs(k[Blends] - a[cats]) = 1 /\
  % 11. The man who keeps horses lives next to the man who smokes Dunhill.
  abs(a[horses] - k[Dunhill]) = 1 /\
  % 14. The Norwegian lives next to the blue house.
  abs(s[Norwegian] - c[blue]) = 1 /\
  % 15. The man who smokes Blends has a neighbour who drinks water.
  abs(k[Blends] - d[water]) = 1


;

output
[
 "s: " ++ show(s) ++ "\n" ++
 "a: " ++ show(a) ++ "\n" ++
 "d: " ++ show(d) ++ "\n" ++
 "k: " ++ show(k) ++ "\n" ++
 "c: " ++ show(c) ++ "\n" 

];