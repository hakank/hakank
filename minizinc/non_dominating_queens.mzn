% 
% Non dominating queens puzzle in MiniZinc.
%
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles4.html, puzzle nr. 6.
% Description  : Non-dominating queens problem
% Source       : http://www.cli.di.unipi.it/~velucchi/queens.txt

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol4s6.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: size = 5;
set of 1..size: S = 1..size;

% var x(S,S) binary;  #  x(i,j) = 1 if square (i,j) occupied, 0 otherwise
array[S,S] of var 0..1: x;
% var a(S,S) binary;  # a(i,j) = 1 if square (i,j) attacked, 0 otherwise
array[S,S] of var 0..1: a;

var int: numa = sum(i in S, j in S) (a[i,j]);

% minimize number of squares attacked or occupied
% solve minimize numa;
solve :: int_search(
        [a[i,j] | i,j in S] ++ [x[i,j] | i,j in S], 
        first_fail, 
        indomain_min, 
        complete) 
    minimize numa;

constraint
  % number of pieces placed equals size of board
  sum(i in S,j in S) (x[i,j]) = size

  /\  % a[i,j) = 1 if square (i,j) attacked or occupied
  forall(i in S, j in S) (
      (sum(m in S where m != i /\ m-i+j >= 1 /\ m-i+j <= size) (x[m,m-i+j]) +
       sum(m in S where m != i /\ i+j-m >= 1 /\ i+j-m <= size) (x[m,i+j-m]) +
       sum(m in S) (x[m,j]) + 
       sum(m in S  where  m != j) (x[i,m]) +
       x[i,j]
      ) 
       <= size*a[i,j]
  )
  /\ % a[i,j) = 0 if square (i,j) not attacked or occupied
  forall(i in S,j in S) (
       (sum(m in S  where  m != i /\ m-i+j >= 1 /\ m-i+j <= size) (x[m,m-i+j]) +
        sum(m in S  where  m != i /\ i+j-m >= 1 /\ i+j-m <= size) (x[m,i+j-m]) +
        sum(m in S) (x[m,j]) + 
        sum(m in S  where  m != j) (x[i,m]) +
        x[i,j]) 
        >= a[i,j] 
  )
;

output 
[
 "\nnuma: " ++ show(numa)  ++ "\n"
]
++
[
   if j = 1 /\ i = 1 then 
     "\n: x\n"
   else "" endif ++
   if j = 1 then "\n" else " " endif ++
   show(x[i,j]) 
   | i,j in 1..size
] ++ 
[
   if j = 1 /\ i = 1 then 
     "\n: a\n"
   else "" endif ++
   if j = 1 then "\n" else " " endif ++
   show(a[i,j]) 
   | i,j in 1..size
] ++ ["\n"]
;

