% 
% Ice cream production in MiniZinc.
% 
% From "Understanding and Using Linear Programming", page 16f.
% Ice cream production during a year given monthly demands.
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% MiniZinc/mip gives the following solution with storage cost 20
% obj = 53180;
% s = [0, 0, 10, 12, 6, 0, 94, 37, 0, 110, 130, 190, 0];
% x = [0, 340, 340, 452, 644, 644, 644, 643, 643, 460, 460, 460, 460];
% y = [0, 340, 0, 112, 192, 0, 0, 0, 0, 0, 0, 0, 0];
% z = [0, 0, 0, 0, 0, 0, 0, 1, 0, 183, 0, 0, 0];


set of int: year = 0..12;
array[year] of int: d; % demand
int: storage_cost = 20;

% var
array[year] of var int: x; % production
array[year] of var int: s; % surplus
array[year] of var int: y; % increase from month i-1 to month i
array[year] of var int: z; % decrease from month i-1 to month i


var int: obj = 
             50*sum(i in year) (y[i]) + 
             50*sum(i in year) (z[i]) + 
             storage_cost*sum(i in year) (s[i])
;

solve minimize obj;
% solve :: int_search(x ++ s ++ y ++ z ++ [obj], first_fail, indomain_middle, complete) minimize obj;

constraint
  x[0] = 0
  /\
  s[0] = 0
  /\
  s[12] = 0
  /\
  forall(i in 1..12) (
     x[i] + s[i-1] - s[i] = d[i]
  )
  /\
  forall(i in 1..12) (
     x[i] - x[i-1] = y[i] - z[i]
  )
  /\
  forall(i in year) (
     x[i] >= 0
     /\
     s[i] >= 0
     /\
     z[i] >= 0
     /\
     y[i] >= 0
  )
;

%
% data
%

% demand
%       jan  feb  mar  apr  may  jun  jul  aug  sep  oct nov   dec 
d = array1d(year, [0, 340, 330, 450, 650, 650, 550, 700, 680, 350, 440, 400, 650]);


output
[
   "x: " ++ show(x) ++ "\n" ++
   "s: " ++ show(s) ++ "\n" ++
   "y: " ++ show(y) ++ "\n" ++
   "z: " ++ show(z) ++ "\n" ++
   "obj: " ++ show(obj) ++ "\n" 
];
