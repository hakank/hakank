% 
% Gathering 4 Garder matrix problem in MiniZinc.
% 
% Problem by Bernardo Recamán
%
% 2013-10-21 via the G4G Hangout https://twitter.com/G4GCelebration
% Here's the grid
% http://ow.ly/i/3ukNk
% 
% Place 1..20 so that there are exactly two numbers per each row and
% two per each columns, such that either the sum or the product 
% equals the value in the margins.
% 
% This is the unique solution:
% 
% rows: [13, 26, 33, 36, 40, 42, 44, 48, 50, 72]
% cols: [17, 20, 24, 25, 32, 57, 65, 66, 80, 84]
% 
%  1  0  0  0  0  0 13  0  0  0
%  0  0  0  0 14  0  0  0  0 12
%  0  0 15  0 18  0  0  0  0  0
%  0  0  0 17  0 19  0  0  0  0
%  0  2  0  0  0  0  0  0 20  0
%  0  0  0  0  0  0  0  6  0  7
%  0  0  0  0  0  0  0 11  4  0
% 16  0  0  0  0  3  0  0  0  0
%  0 10  0  0  0  0  5  0  0  0
%  0  0  9  8  0  0  0  0  0  0
% x_rows: [1, 13, 14, 12, 15, 18, 17, 19, 2, 20, 6, 7, 11, 4, 16, 3, 10, 5, 9, 8]
% x_cols: [1, 16, 2, 10, 15, 9, 17, 8, 14, 18, 19, 3, 13, 5, 6, 11, 20, 4, 12, 7]
% ----------
% ==========
% 
% (Solved in 0.85s by Chuffed, in 4.1s by G12 lazy)
%
%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 10;

array[1..n] of int: rows = [13,26,33,36,40,42,44,48,50,72];
array[1..n] of int: cols = [17,20,24,25,32,57,65,66,80,84];

% decision variables
array[1..n, 1..n] of var 0..20: x;

array[1..n,1..2] of var 1..20: x_rows;
array[1..n,1..2] of var 1..20: x_cols;

% solve satisfy;
solve :: int_search([x[i,j] | i,j in 1..n] 
                    % ++ 
                    %[x_rows[i,j] | i in 1..n, j in 1..2] ++ 
                    % [x_cols[i,j] | i in 1..n, j in 1..2]
                    ,
   first_fail, indomain_split, complete) satisfy;

constraint
   forall(i in 1..n) (
      sum([bool2int(x[i,j] > 0) | j in 1..n]) = 2
      /\
      sum([bool2int(x[j,i] > 0) | j in 1..n]) = 2

      /\ % rows, identify the two numbers
      exists(k in 1..n) (
         forall(s in 1..k-1) ( x[i,s] = 0) /\
         x_rows[i,1] = x[i,k] 
      )
      /\
      exists(k in 1..n) (
         forall(s in k+1..n) ( x[i,s] = 0) /\
         x_rows[i,2] = x[i,k] 
      )
      /\ % columns, identify the two numbers
      exists(k in 1..n) (
         forall(s in 1..k-1) ( x[s,i] = 0) /\
         x_cols[i,1] = x[k,i] 
      )
      /\
      exists(k in 1..n) (
         forall(s in k+1..n) ( x[s,i] = 0) /\
         x_cols[i,2] = x[k,i] 
      )
      /\
      % now, either sum or product
      (
       sum([x_rows[i,j] | j in 1..2]) = rows[i]
       \/
       product([x_rows[i,j] | j in 1..2]) = rows[i]
      )
      /\
      (
       sum([x_cols[i,j] | j in 1..2]) = cols[i]
       \/
       product([x_cols[i,j] | j in 1..2]) = cols[i]
      )
   )
   /\
   forall(k in 1..20) (
     count([x[i,j] | i,j in 1..n], k, 1) % :: domain
     % /\
     % sum([bool2int(x[i,j] == k) | i,j in 1..n]) = 1
   )
   % /\ alldifferent_except_0([x[i,j] | i,j in 1..n]) % :: domain
;

output 
[
  "rows: " ++ show(rows) ++ "\n" ++
  "cols: " ++ show(cols) ++ "\n"
]
++
[
  if j = 1 then "\n" else " " endif ++
    show_int(2, x[i,j])
  | i,j in 1..n
]
++
[ 
  "\nx_rows: " ++ show(x_rows) ++ "\n" ++
  "x_cols: " ++ show(x_cols) ++ "\n"
]
;

