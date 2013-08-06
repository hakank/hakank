% 
% Table seating problem in MiniZinc.
% 
% From problem formulation from
% Fei Dai, Mohit Dhawan, Changfu Wu and Jie Wu
% "On Solutions to a Seating Problem", page 1
% """
% A classical [...] seating problem is the following: 
% n couples are seated around a table in such a way
% that two neighbors of each person k are the different gender
% of k and they are not the spouse of k. 
% """


% Coding of a couple i:
%   female: 2*(i-1)+1
%   male  : 2*(i-1)+2


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 3; % number of couples
int: m = 2*n; % number of persons

% The couples
array[1..n] of set of int: couples = [{2*(i-1)+1, 2*(i-1)+2} | i in 1..n];

array[0..1] of string: gender_str = array1d(0..1, ["h","w"]);

% decision variable
% the seating
array[1..m] of var 1..m: x;
% the gender
array[1..m] of var 0..1: gender;

% Using the set representation of couples.
predicate is_couple(int: n, var int: a, var int: b, array[int] of set of int: c) =
  exists(j in 1..n) (
     a in c[j] /\ b in c[j]
  )
;

% solve satisfy;
solve :: int_search(x, first_fail, indomain_split, complete) satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;


% Print the couples
constraint 
  forall(i in 1..n) (
    trace("couple " ++ show(i) ++ ": (" ++ show(2*(i-1)+1) ++ "," ++ show(2*(i-1)+2) ++ ")\n", 1=1)
  )
  /\
  trace("\n", 1=1)
;


constraint
  alldifferent(x)
;

% symmetry breaking
% constraint x[1] = 1 \/ x[1] = 2;


% the gender
constraint 
  forall(i in 1..m) (
     gender[i] = x[i] mod 2
  )
;


% The neighbours must be either 
%  - of the different gender
%  - not the spouse
constraint
   forall(i in 2..m) (
      gender[i] != gender[i-1] 
      /\
      not(is_couple(n, x[i], x[i-1], couples))
    )
   /\ % around the corner
   gender[m] != gender[1] 
   /\
   not(is_couple(n, x[m], x[1], couples))
;


%
% output 
%
output [
 "x     : " ++ show(x) ++ "\n" ++ 
 "gender: " ++ show(gender) ++ "\n" ++
 "gender2:" 
] ++ [
  gender_str[fix(gender[i])] 
  | i in 1..m
] ++ ["\n"];