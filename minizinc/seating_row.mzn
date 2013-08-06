% 
% Movie theater seating problem in MiniZinc.
% 
% Row seating problem.
% 
% Problem formulation from
% Fei Dai, Mohit Dhawan, Changfu Wu and Jie Wu
% "On Solutions to a Seating Problem", page 1
% """
% n couples are seated in one row. Each person, except two 
% seated at the two ends, has two neighbors. It is required 
% that each neighbor of person k should either have the 
% same gender or be the spouse of k.
% """
%
% Reasonable pattern (from the same page as above):
% """
% A reasonable pattern is an assignment of n couples 
% without indices that meet the above requirement. 
% For example, when n = 3 all reasonable patterns 
% are as follows:
%   wwwhhh whhwwh whhhww hhwwwh
%   hhhwww hwwhhw hwwwhh wwhhhw
% """

% Note: This model don't really care about the reasonable patterns
% since I'm interested in the concrete seating placements.
% For n=3 there is a total of 60 patterns.
% Here are all 8 different patterns generated (gender2) for n=3.
% The first number indicates the occurences of each pattern. 
% 
% 12 gender2:hhhwww
%  6 gender2:hhwwwh
%  6 gender2:hwwhhw
%  6 gender2:hwwwhh
%  6 gender2:whhhww
%  6 gender2:whhwwh
%  6 gender2:wwhhhw
% 12 gender2:wwwhhh
%

%
% Coding of a couple i:
%   husband: 2*(i-1)+1   = 0
%   wife  : 2*(i-1)+2    = 1
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 3; % number of couples
int: m = 2*n; % number of persons

% the couple table (used in couple/4)
% Generate the couples.
array[1..n] of set of int: couples = [{2*(i-1)+1, 2*(i-1)+2} | i in 1..n];

% String of gender (for the minizinc wrapper/solver)
array[0..1] of string: gender_str = array1d(0..1, ["h","w"]);


% decision variable
array[1..m] of var 1..m: x;
array[1..m] of var 0..1: gender;

% Using the set representation of couples.
predicate is_couple(int: n, var int: a, var int: b, array[int] of set of int: c) =
  exists(j in 1..n) (
     a in c[j] /\ b in c[j]
  )
;


% Another representation of the couples.
predicate is_couple1(int: n, var int: a, var int: b) =
  % exists(i in 1..n) (
  exists(i in max(lb(a), lb(b))..min(ub(a), ub(b))) (
     (2*(i-1)+1 = a /\ 2*(i-1)+2 = b) 
     \/
     (2*(i-1)+1 = b /\ 2*(i-1)+2 = a) 
  )
;


% solve satisfy;
solve :: int_search(x, first_fail, indomain_split, complete) satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;


% Print the couples
constraint
  forall(i in 1..n) (
     trace("couple " ++ show(i) ++ ": " ++ show(couples[i]) ++ "\n", 1=1)
  )
;

constraint
  alldifferent(x) :: domain
;

% some symmetry breaking
% constraint 
%    (x[1] = 1 \/ x[1] = 2)
% ;

% get the gender
constraint 
  forall(i in 1..m) (
     gender[i] = x[i] mod 2
  )
;


% The neighbour must be either 
%  - of the same gender
%  - or the spouse
constraint
   forall(i in 2..m) (
     (gender[i] = gender[i-1])
     \/
     is_couple(n, x[i], x[i-1], couples)
     % is_couple1(n, x[i], x[i-1]) % alternative
  )
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