% 
% Leap year in MiniZinc.
% 
% http://en.wikipedia.org/wiki/Leap_year

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

% decision variables
var 1..2014: year;
var 0..1: is_leap_year;


predicate leap_year(var int: y) =
  (y mod 4 == 0 /\ y mod 100 != 0)
  \/
  y mod 400 == 0
;


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  % if leap_year(year) then
  %    is_leap_year == 1
  % else 
  %    is_leap_year == 0
  % endif

  is_leap_year = 1 <-> leap_year(year)

;

output [
  "year: ", show(year), "\n",
  "is_leap_year: ", show(is_leap_year), "\n"
];

