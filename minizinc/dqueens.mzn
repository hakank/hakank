% 
% Dudeney's queen placement problem puzzle in MiniZinc.
%
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles2.html, puzzle nr. 11
% Description  : Dudeney's queen placement problem
% Source       : Dudeney, H.E., (1917), Amusements in Mathematics, Thomas Nelson and Sons.

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol2s11.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: size = 8;
array[1..size,1..size] of var 0..1: x; % 1 if square {I,J} occupied, 0 otherwise
array[1..size,1..size] of var 0..1: a; % 1 if square {I,J} attacked, 0 otherwise
var 0..100: suma = sum(i, j in 1..size) (a[i,j]);

% minimise number of squares attacked  
solve minimize suma;
% solve :: int_search([ a[i,j] | i,j in 1..size ], "first_fail", "indomain", "complete") minimize suma;

constraint
   % all eight queens used
    sum(i in 1..size,j in 1..size) (x[i,j]) = 8
   /\
   % five of original queens untouched
   sum(j in 3..size) (x[8,j] + x[7,size] + x[6,size]) = 5

   /\
   % a(i,j) = 1 if square (i,j) attacked
   forall(i, j in 1..size) (
     (
        sum(m in 1..size where m != i /\ m-i+j >= 1 /\ m-i+j <= size) (x[m,m-i+j]) +
        sum(m in 1..size where m != i /\ i+j-m >= 1 /\ i+j-m <= size) (x[m,i+j-m]) +
        sum(m in 1..size where m != i) (x[m,j]) + 
        sum(n in 1..size where n != j) (x[i,n]) 
      )  <= 99*a[i,j]
     )
;


output [
   if i = 1 /\ j = 1 then 
    "\nsuma: " ++ show(suma) ++
    "\nx:" 
   else "" endif ++
   if j = 1 then "\n" else " " endif ++
   show(x[i,j])
   | i in 1..size, j in 1..size

] ++ [
   if i = 1 /\ j = 1 then 
    "\na:" 
   else "" endif ++
   if j = 1 then "\n" else " " endif ++
   show(a[i,j])
   | i in 1..size, j in 1..size

] ++ ["\n"]
;
