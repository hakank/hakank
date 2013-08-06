% 
% Letter square problem in MiniZinc.
% 
% This problem is from the swedish book 
% Paul Vaderlind: "Vaderlinds nya hjärngympa"
% ('Vaderlind's new brain gymnastics'), page 63ff.
% 
% I don't know the origin of the problem.
%
% The objective is to create a matrix where all 
% values on each row/column is different (except for
% the blanks). A set of hints is given: 
% The (or some) first seen non blank of each 
%  - upper row
%  - lower row
%  - left column
%  - right column
% seen from that view.
%
%     B B
%   -------
%  |A   B C| 
% B|  B C A| 
% B|B C A  | 
%  |C A   B| 
%  --------
%   C
% 
% Note: there are not hints for the right column.
% 
% This model codes the hints as follows:
%  blank -> 0
%  A     -> 1
%  B     -> 2
%  etc.
% 
% row_upper = [0,2,2,0];
% row_lower = [3,0,0,0];
% col_left  = [0,2,0,0];
% col_right = [0,0,0,0];
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

int: n; % size of square
int: m; % numbers to place in each row/column
int: num_zeros = n-m; % number of zeros

array[1..n, 1..n] of var 0..m: x;

% hints, problem 67
% This is the first "visible" number except empty square
array[1..n] of 0..n: row_upper;
array[1..n] of 0..n: row_lower;
array[1..n] of 0..n: col_left;
array[1..n] of 0..n: col_right;


predicate all_different_except_0(array[int] of var int: x) =
   forall(i,j in index_set(x) where i != j) (
        (x[i] != 0 /\ x[j] != 0) -> x[i] != x[j]      
   )
;

% solve satisfy;
solve :: int_search([x[i,j] | i,j in 1..n], smallest, indomain_min, complete) satisfy;

constraint

    forall(i in 1..n) (
      all_different_except_0([x[i,j] | j in 1..n]) /\
      all_different_except_0([x[j,i] | j in 1..n]) 
      /\
      sum(j in 1..n) (bool2int(x[i,j] = 0)) = num_zeros /\
      sum(j in 1..n) (bool2int(x[j,i] = 0)) = num_zeros
    )
    /\
    forall(i in 1..n) (
      if row_upper[i] > 0 then 
          exists(j in 1..n) (
            forall(k in 1..j-1) (x[k,i] = 0) /\
            x[j,i] = row_upper[i])
      else 
        true 
      endif
      /\

      if row_lower[i] > 0 then 
         exists(j in 1..n) (
           forall(k in j+1..n) (x[k,i] = 0) /\
           x[j,i] = row_lower[i])
      else 
         true 
      endif

      /\
      if col_left[i] > 0 then 
          exists(j in 1..n) (
            forall(k in 1..j-1) (x[i,k] = 0) /\
            x[i,j] = col_left[i])
      else 
        true 
      endif

      /\
      if col_right[i] > 0 then 
          exists(j in 1..n) (
            forall(k in j+1..n) (x[i,k] = 0) /\
            x[i,j] = col_right[i])
      else 
        true 
      endif

    )
;

output 
[
 if j = 1 then ",\n" else "," endif ++
   show(x[i,j])
 |i,j in 1..n
];

% output 
% [
%    if row_upper[j] > 0 then 
%      "f(" ++ show(row_upper[j]) ++ ")"
%    else
%      "  -"
%    endif ++ ","
%    | j in 1..n+1
% ]
% ++
% [
%   if j = 1 then "\n" else "," endif ++
%     if j = 1 then
%        if col_left[i] > 0 then 
%           "f(" ++ show(col_left[i]) ++ ")," 
%         else "  -," endif 
%     else "" endif ++
%       show(x[i,j]) ++
%     if j = n then 
%       if col_right[i] > 0 then 
%          ", f(" ++ show(col_right[i]) ++ ")," 
%          else ",  -," endif
%     else "" endif
%   | i,j in 1..n
% ] ++ 
% ["\n"] ++
% [
%  if row_lower[j] > 0 then 
%      "f(" ++ show(row_lower[j]) ++ ")"
%    else
%      "-"
%    endif ++ ","
%    | j in 1..n+1
% ]
% ++
% ["\n"];
