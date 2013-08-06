% 
% Logic Puzzle from The Art of Prolog in MiniZinc.
% 
% 
% Sterling, Shapiro: The Art of Prolog (2nd edition), page 258
% """
% Three friends come first, second, and third in a programming competition.
% Each of the three has a different first name, likes a different sport,
% and has a different nationality.
% Michael likes baskeball and did better than the American,
% Simon, the Israeli, did better than the tennis player.
% The cricket player came first.
% Who is the Australian? What sport does Richard play?
% """
% 
% Answer: Michael is the Australian and Richard plays tennis.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
set of int: d = 1..3;

d: michael = 1;
d: simon = 2;
d: richard = 3;

var d: basketball;
var d: tennis;
var d: cricket;
array[d] of var d: sport = [basketball, tennis, cricket];

var d: america;
var d: israel;
var d: australia;
array[d] of var d: country = [america, israel, australia];

array[d] of var d: place;


solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint
  all_different(sport)
  /\
  all_different(country)
  /\
  all_different(place)
  
  % Michael likes baskeball and did better than the American,
  /\
  michael = basketball
  /\
  place[michael] < place[america]

  % Simon, the Israeli, did better than the tennis player.
  /\
  simon = israel
  /\
  place[simon] < place[tennis]

  % The cricket player came first.
  /\
  place[cricket] = 1

;

output [
  "[michael, simon, richard]: ", show([michael, simon, richard]),"\n",
  "sport: ", show(sport), "\n",
  "country: ", show(country), "\n",
  "place: ", show(place), "\n",
]
;