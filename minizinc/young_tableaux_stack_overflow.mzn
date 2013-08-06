% 
% Young tableaux in MiniZinc.
% 
% See 
% http://mathworld.wolfram.com/YoungTableau.html
% and
% http://en.wikipedia.org/wiki/Young_tableau
% """
% The partitions of 4 are
%  {4}, {3,1}, {2,2}, {2,1,1}, {1,1,1,1}
%
% And the corresponding standard Young tableaux are:
%
% 1.   1 2 3 4
%
% 2.   1 2 3         1 2 4    1 3 4
%      4             3        2
%
% 3.   1 2           1 3
%      3 4           2 4
%
% 4    1 2           1 3      1 4 
%      3             2        2 
%      4             4        3
%
% 5.   1
%      2
%      3
%      4
% """  
% 

% This Stack Overflow question has some related questions
% "Young tableaux in C"
% http://stackoverflow.com/questions/15705001/young-tableaux-in-c
% """
% Consider a [6,3,2,1,1,1] Ferrers diagram:
% 1) If 6 is fixed on the 6th box of the 1st row and 11 is fixed in the 
%    last box of the 1st column, in how many ways can you complete the 
%    diagram in a standard way?
% 
% 2) If 7 is fixed on the 6th box of the 1st row and 11 is fixed in the 
%    last box of the 1st column, in how many ways can you complete the 
%    diagram in a standard way?
% 
% 3) If 8 is fixed on the 6th box of the 1st row and 11 is fixed in 
%    the last box of the 1st column, in how many ways can you complete the 
%    diagram in a standard way?"
% """

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

%
% The empty cells are represented as the number n+1 for simplifying the
% comparisons. In the output n+1 is replaced with 0 or "" using fix.
%
int: n = 14;
array[1..n, 1..n] of var 1..n+1: x;
array[1..n] of var 0..n+1: p; % the partition structure


% solve satisfy;
solve :: int_search([x[i,j] | i,j in 1..n], first_fail, indomain_min, complete) satisfy;

% copy arrays of 1d where both arguments are var int
predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


constraint
   % 1..n is used exactly once
   forall(i in 1..n) (
     count([x[i,j] | i,j in 1..n], i, 1)
   )
   /\
   x[1,1] = 1

   /\ % row wise
   forall(i in 1..n) (
      forall(j in 2..n) (
        x[i,j] >= x[i,j-1]
      )
   )
   /\ % column wise
   forall(j in 1..n) (
     forall(i in 2..n) (
       x[i,j] >= x[i-1,j]
     )
   )
   /\ % calculate the structure (the partition)
   forall(i in 1..n) (
      p[i] = sum(j in 1..n) (bool2int(x[i,j] <= n))
   )
   /\
   sum(p) = n
   /\
   forall(i in 2..n) (
      p[i-1] >= p[i]
   )
   % /\ % show just for a specific partition
   /\
   cp1d(p, [6,3,2,1,1,1,0,0,0,0,0,0,0,0])

  % 1) If 6 is fixed on the 6th box of the 1st row and 11 is fixed in the 
  %    last box of the 1st column, in how many ways can you complete the 
  %    diagram in a standard way?
  % Answer: There are two such solutions:
  %  1  2  3  4  5  6        
  %  7 12 13           
  %  8 14            
  %  9             
  % 10             
  % 11
  % ----------
  %  1  2  3  4  5  6        
  %  7 12 14           
  %  8 13            
  %  9             
  % 10             
  % 11
  /\ x[1,6] = 6  

  % 
  % 2) If 7 is fixed on the 6th box of the 1st row and 11 is fixed in the 
  %    last box of the 1st column, in how many ways can you complete the 
  %    diagram in a standard way?
  %    Answer: There are 10 such solutions
  % /\ x[1,6] = 7
  
  % 
  % 3) If 8 is fixed on the 6th box of the 1st row and 11 is fixed in 
  %    the last box of the 1st column, in how many ways can you complete the 
  %    diagram in a standard way?"
  %    Answer: There are 30 such solutions
  % /\ x[1,6] = 8

  % ensure that 11 is the last box in first column
  /\ exists(j in 1..n) (x[j,1] = 11 /\ forall(k in j+1..n) (x[k,1] = n+1))


;

output 
[
  "\np: ", show(p)
] ++
[
 if fix(x[i,j]) <= n /\ j = 1 then "\n" else " " endif ++
  if fix(x[i,j]) <= n then show_int(2, x[i,j]) else "" endif
  | i,j in 1..n
] ++ ["\n"];