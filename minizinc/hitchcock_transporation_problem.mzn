%
% (Hitchcock-Koopmans) Transportation problem in Minizinc
% 
% Given sum of rows and columns, calculate a minimum transportation matrix.
% 
% Data and principal model from
% http://www.math.niu.edu/~rusin/known-math/99/transport
%
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: num_rows;
int: num_cols;

% General solution
set of 1..num_rows:  rows = 1..num_rows;
set of 1..num_cols: cols = 1..num_cols;
int: min_val;

array[rows, cols] of var int: x; % the transportation matrix
array[rows] of int: rr;          % row sums
array[cols] of int: cc;          % column  sums

var int: total = sum(i in rows, j in cols) (x[i,j]);

% solve satisfy;
solve ::int_search ([ x[r,c] | r in rows, c in cols], first_fail, indomain_min, complete) minimize total;

constraint
%  total <= 2651 % for satisfy
%  /\
  % sum rows 
  forall(i in rows) ( 
     sum(j in cols) (x[i,j]) = rr[i] 
  )
  /\ % sum cols
  forall(j in cols) (
    sum(i in rows) (x[i,j]) = cc[j]
  )
  /\
  % minimum value in the transportation matrix
  forall(i in rows, j in cols) (
    x[i,j] >= min_val
  )
;


%
% data 
%

num_rows = 5;
num_cols = 5;

min_val = 0; % minimum value
rr = [101, 200, 350, 1400, 600];
cc = [1250, 200, 300, 401, 500];


output [
   if i = 1 /\ j = 1 then
    "\n" ++ show(x) ++ "\n" ++ "\ntotal:" ++ show(total) ++ "\n"
   else "" endif ++
   if j = 1 then "\n" else " " endif ++
   show(x[i,j]) 
   | i in rows, j in cols
  
] ++ ["\n"];

