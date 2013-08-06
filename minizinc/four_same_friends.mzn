% 
% Four alike pals problem in MiniZinc.
% 
% From the Swedish blog post "Fyra lika kompisar" (Four alike pals):
% http://mattebloggen.com/2012/12/fyra-lika-kompisar/
% (translated by me)
% """
% Four friends are alike in many ways: for all pairs,
% the pair has the same first name or the same name or the same 
% birth date. 
% However, no three of the friends has the same first name, 
% no three have the same last name, nor there are there three with the 
% same birth date.
% 
% Can this kind of four friends exists?
% """

% Solution:
% 
% If we allow for 1..n first names/last names/birthdays, there are 10368 
% different solutions.
%
% The smallest number of first names/last names/birthdays is 2 (the z value
% which is then minimized).
%
%  first_name: [1, 2, 2, 1]
%  last_name : [2, 2, 1, 1]
%  birthday  : [2, 1, 2, 1]
%  z         : 2
% 
% There are 48 different solutions with 2 different first names/last names/birthdays.


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
int: n = 4;

array[1..n] of int: cover = array1d(1..n, set2array(1..n));
array[1..n] of int: limit_low = [0 | i in 1..n];
array[1..n] of int: limit_high = [2 | i in 1..n];


% decision variables
array[1..n] of var 1..n: first_name;
array[1..n] of var 1..n: last_name;
array[1..n] of var 1..n: birthday;

var 1..n: z = max(first_name ++ last_name ++ birthday);

% solve satisfy;
solve minimize z;
% solve :: int_search(first_name ++ last_name ++ birthday, input_order, indomain, complete) satisfy;

constraint
  % same pair 
  forall(i, j in 1..n where i < j) (
     first_name[i] = first_name[j] \/
     last_name[i] = last_name[j] \/
     birthday[i] = birthday[j]
  )
  % /\ % not three's (42244 failures)
  % forall(i,j,k in 1..n where i < j /\ j < k) (
  %    not(first_name[i] = first_name[j] /\ first_name[i] = first_name[k] /\ first_name[j] = first_name[k]) /\
  %    not(last_name[i] = last_name[j] /\ last_name[i] = last_name[k] /\ last_name[j] = last_name[k]) /\
  %    not(birthday[i] = birthday[j] /\ birthday[i] = birthday[k] /\ birthday[j] = birthday[k])
  % )
  % Better version (970 failures)
  /\ global_cardinality_low_up(first_name, cover, limit_low, limit_high)
  /\ global_cardinality_low_up(last_name , cover, limit_low, limit_high)
  /\ global_cardinality_low_up(birthday  , cover, limit_low, limit_high)


  % /\ z = 2 % for solve satisfy with the minimum number of different first name/last names/birthdays
;


output [

  "first_name: " ++ show(first_name) ++ "\n" ++
  "last_name : " ++ show(last_name) ++ "\n" ++
  "birthday  : " ++ show(birthday) ++ "\n" ++
  "z         : " ++ show(z) ++ "\n"

];

