% 
% Houses problem in MiniZinc.
% 
% From Kanren example
% http://kanren.cvs.sourceforge.net/viewvc/kanren/kanren/examples/houses.scm?view=markup
% """
% Taken from _Algebra 1_, Glencoe/McGraw-Hill, New York, New York, 1998
% pg. 411, Problem 56
%
% There are 8 houses on McArthur St, all in a row. These houses
% are numbered from 1 to 8.
%
% Allison, whose house number is greater than 2, lives next door
% to her best friend, Adrienne. Belinda, whose house number is
% greater than 5, lives 2 doors away from her boyfriend, Benito.
% Cheri, whose house number is greater than Benito's, lives
% three doors away from her piano teacher, Mr. Crawford. Daryl,
% whose house number is less than 4, lives 4 doors from his
% teammate, Don. Who lives in each house?
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn";

int: n = 8;
var 1..n: allison;
var 1..n: belinda;
var 1..n: adrienne;
var 1..n: benito;
var 1..n: cheri;
var 1..n: crawford;
var 1..n: daryl;
var 1..n: don;

% where does each person live
array[1..n] of var 1..n: all = [allison, belinda, adrienne, benito, 
                          cheri, crawford, daryl, don];
array[1..n] of string: all_str = ["allison", "belinda", "adrienne", "benito", 
                          "cheri", "crawford", "daryl", "don"];

% who lives in which house
array[1..n] of var 1..n: houses;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   alldifferent(all)
   /\

   % Allison, whose house number is greater than 2, lives next door
   % to her best friend, Adrienne. 
   allison > 2
   /\
   abs(allison-adrienne) = 1

   % Belinda, whose house number is greater than 5, 
   % lives 2 doors away from her boyfriend, Benito.
   /\ 
   belinda > 5
   /\
   abs(belinda-benito) = 2

   % Cheri, whose house number is greater than Benito's, lives
   % three doors away from her piano teacher, Mr. Crawford. 
   /\
   cheri > benito
   /\
   abs(cheri-crawford) = 3

   % Daryl, whose house number is less than 4, 
   % lives 4 doors from his teammate, Don. 
   /\
   daryl < 4
   /\
   abs(daryl-don) = 4


   /\ % channeling the persons and houses, i.e.
      % place each person in proper house
   inverse(all, houses)
;

output [
  "all   : " ++ show(all) ++ "\n" ++
  "houses: " ++ show(houses) ++ "\n"
]
++
[
  "house: " ++ show(i) ++ ": " ++ show(all_str[fix(houses[i])]) ++ "\n"
  | i in 1..n
]
 ++ ["\n"]
;

