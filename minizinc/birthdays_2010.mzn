% 
% Birthdays puzzle in MiniZinc.
%
% This is a true story:
%
% In this year 2010 my older brother Anders will be the same 
% age as the year I was born in the last century, and I will be 
% the same age as the year he was born in the last century.
% My brother is four years older than me.
%
% How old are we this year?
%
% Note: Of course, this could be modelled in a much easier way.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
var 1..100: hakank_born;
var 1..100: anders_born;
var 1..100: hakank_age;
var 1..100: anders_age;

var 100..200: this_year;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   hakank_age = this_year - hakank_born /\
   anders_age = this_year - anders_born /\

   hakank_born > anders_born /\

   hakank_age = anders_born /\ 
   anders_age = hakank_born /\

   hakank_born - anders_born = 4 /\

   this_year = 110

;

output
[
  "hakank_age: " ++ show(hakank_age) ++ "\n" ++
  "anders_age: " ++ show(anders_age) ++ "\n" ++
  "hakank_born: " ++ show(hakank_born) ++ "\n" ++
  "anders_born: " ++ show(anders_born) ++ "\n" ++
  "this_year: " ++ show(this_year)
];