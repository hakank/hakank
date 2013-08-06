%
% Smullyan's Lion and Unicorn problem in MiniZinc.
%
% From Raymond Smullyans "What is the name of this book?",
% Chapter 4 "Alice in the Forest of Forgetfulness".
% Problems 47, 48, 49, 50.
%
% Lions lies on Mondays, Tuesdays, and Wednesdays.
% Unicorns lies on Thursdays, Fridays, and Saturdays.

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
% include "globals.mzn"; 

% Truthfullness of lion and unicorn.
% start week with Monday (day 0)
array[1..7] of bool: unicorn = [ true,  true,  true, false, false, false, true];
array[1..7] of bool: lion    = [false, false, false,  true,  true,  true, true];

% the decision variable
var 0..6: today;

predicate says(var bool: when, var bool: what) =
   (when = true /\ what = true) 
   \/
   (when = false /\ what = false) 
;


solve satisfy;

constraint
  

   %% Problem 47
   %% Lion: Yesterday was one of my lying days.
   %% Unicorn: Yesterday was one of my lying days too.
   %% Solution: Today is Thursday (day 3)
   %says(lion[1+today], lion[1+ ((today-1) mod 7)] = false)
   %/\
   %says(unicorn[1+today], unicorn[1+((today-1) mod 7)] = false)



   %% Problem 48
   %% Lion: 
   %%   1) I lied yesterday
   %%   2) I will lie two days after tomorrow
   %% Solution: Today is Monday (day 0)
   % says(lion[1+today], lion[1+(today - 1) mod 7] = false)
   %/\
   %says(lion[1+today], lion[1+(today + 3) mod 7 ] = false)


   %% Problem 49
   %% Lion: 
   %%   1) I lied yesterday
   %%   2) I will lie again tomorrow
   %% Solution: On no day is this possible to say.
   %says(lion[1+(today mod 7)], lion[1+((today - 1 ) mod 7)] = false)
   %/\
   %says(lion[1+(today mod 7)], lion[1+((today + 1) mod 7)] = false)



   %% Problem 50
   %% Lion: I lied yesterday and I will lie again tomorrow
   %% Solution: Today is either Monday or Wednesday.
   says(lion[1+today], lion[1+((today - 1) mod 7)] = false  /\ lion[1+((today + 1) mod 7) ] = false)

;


output [
  "today: ", show(today), "\n"
];
