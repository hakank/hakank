% 
% Schuh's queen placement problem in MiniZinc.
%
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles2.html, puzzle nr. 10.
% Description  : Schuh's queen placement problem
% Source       : Schuh, F., (1943), Wonderlijke Problemen;
%                Leerzam Tijdverdrijf Door Puzzle en Speel, W.J. Thieme & Cie, Zutphen.

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol2s10.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: size = 8;
  
set of 1..size: S = 1..size;
array[S,S] of var 0..1: x; % x(i,j) = 1 if square occupied, 0 otherwise
array[S,S] of var 0..1: a; % a(i,j) = 1 if square attacked, 0 otherwise

var int: numq = sum(i in S,j in S) (x[i,j]);

% maximize number of queens  
solve :: int_search([ x[i,j] | i,j in S], first_fail, indomain_min, complete) maximize numq;

constraint
   sum(i in S,j in S) (x[i,j]) <= size
   /\

  % a(i,j) = 1 if square (i,j) attacked
   forall(i in S,j in S) (
        sum(m in S  where m != i /\ m-i+j >= 1 /\ m-i+j <= size) (x[m,m-i+j]) +
        sum(m in S  where m != i /\ i+j-m >= 1 /\ i+j-m <= size) (x[m,i+j-m]) +
        sum(m in S  where m != i) (x[m,j]) + 
        sum(n in S  where n != j) (x[i,n]) <= 99*a[i,j]
   )
   /\
   % each square either attacked or occupied
   forall(i in S,j in S) (
       a[i,j]+x[i,j] = 1
   )
;

output 
["numq: " ++ show(numq) ++ "\n"] ++
["\nx:"] ++
[
   if j = 1 then "\n" else " " endif ++
   show(x[i,j]) 
   | i,j in S
] ++
["\na:"] ++
[
   if j = 1 then "\n" else " " endif ++
   show(a[i,j]) 
   | i,j in S
]
++
[ "\npositions:" ]
++
[
  if fix(x[i,j]) == 1 then show(j) ++ " " else "" endif
  | i,j in S
]
;
