% 
% A programming puzzle from Einav in MiniZinc.
% 
% From 
% "A programming puzzle from Einav"
% http://gcanyon.wordpress.com/2009/10/28/a-programming-puzzle-from-einav/
% """
% My friend Einav gave me this programming puzzle to work on. Given this array of positive and negative numbers:
% 33   30  -10 -6  18   7  -11 -23   6
% ...
% -25   4  16  30  33 -23  -4   4 -23
%
% You can flip the sign of entire rows and columns, as many of them
% as you like. The goal is to make all the rows and columns sum to positive
% numbers (or zero), and then to find the solution (there are more than one)
% that has the smallest overall sum. So for example, for this array:
% 33  30 -10
% -16  19   9
% -17 -12 -14
% You could flip the sign for the bottom row to get this array:
% 33  30 -10
% -16  19   9
% 17  12  14
% Now all the rows and columns have positive sums, and the overall total is 
% 108.
% But you could instead flip the second and third columns, and the second 
% row, to get this array:
% 33  -30  10
% 16   19    9
% -17   12   14
% All the rows and columns still total positive, and the overall sum is just 
% 66. So this solution is better (I don’t know if it’s the best)
% A pure brute force solution would have to try over 30 billion solutions. 
% I wrote code to solve this in J. I’ll post that separately.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
% include "gecode.mzn";
int: rows;
int: cols;
array[1..rows, 1..cols] of -100..100: data;

array[1..rows, 1..cols] of var -100..100: data_x;

% row/column sums
array[1..rows] of var 0..200: row_sums;
array[1..cols] of var 0..200: col_sums;

% the signs of rows and column
array[1..rows] of var {-1,1}: row_signs;
array[1..cols] of var {-1,1}: col_signs;

% total sum (to minimize)
var 0..1000: total_sum;


% solve minimize total_sum;
solve :: int_search(
        col_sums ++ row_sums ++ row_signs ++ col_signs, 
        occurrence, % first_fail, 
        indomain_max, 
        complete) 
     minimize total_sum;
     % satisfy;

% solve :: relax_and_reconstruct(col_sums ++ row_sums ++ row_signs ++ col_signs,90) minimize total_sum;

constraint

  forall(i in 1..rows, j in 1..cols) (
    data_x[i,j] = data[i,j]*row_signs[i]*col_signs[j]
  )
  /\

  % total sum
  % total_sum = sum([data[i,j]*row_signs[i]*col_signs[j] | i in 1..rows, j in 1..cols ])
  total_sum = sum([data_x[i,j] | i in 1..rows, j in 1..cols ])

  /\ % row sums
  forall(i in 1..rows) (
    row_sums[i] = sum(j in 1..cols) (row_signs[i]*col_signs[j]*data[i,j])
  )

  /\ % column sums
  forall(j in 1..cols) (
    col_sums[j] = sum(i in 1..rows) (row_signs[i]*col_signs[j]*data[i,j])
  )
  % /\ total_sum <= 812 % for solve satisfy
;

% The smaller example above
% rows = 3;
% cols = 3;
% data = array2d(1..rows, 1..cols, [
%  33,  30, -10,
% -16,  19,   9,
% -17, -12, -14
% ]);

%
% The full problem (the corrected one on the blog post)
%
rows = 27;
cols = 9;
data = array2d(1..rows, 1..cols, [
33,30,10,-6,18,-7,-11,23,-6,
16,-19,9,-26,-8,-19,-8,-21,-14,
17,12,-14,31,-30,13,-13,19,16,
-6,-11,1,17,-12,-4,-7,14,-21,
18,-31,34,-22,17,-19,20,24,6,
33,-18,17,-15,31,-5,3,27,-3,
-18,-20,-18,31,6,4,-2,-12,24,
27,14,4,-29,-3,5,-29,8,-12,
-15,-7,-23,23,-9,-8,6,8,-12,
33,-23,-19,-4,-8,-7,11,-12,31,
-20,19,-15,-30,11,32,7,14,-5,
-23,18,-32,-2,-31,-7,8,24,16,
32,-4,-10,-14,-6,-1,0,23,23,
25,0,-23,22,12,28,-27,15,4,
-30,-13,-16,-3,-3,-32,-3,27,-31,
22,1,26,4,-2,-13,26,17,14,
-9,-18,3,-20,-27,-32,-11,27,13,
-17,33,-7,19,-32,13,-31,-2,-24,
-31,27,-31,-29,15,2,29,-15,33,
-18,-23,15,28,0,30,-4,12,-32,
-3,34,27,-25,-18,26,1,34,26,
-21,-31,-10,-13,-30,-17,-12,-26,31,
23,-31,-19,21,-17,-10,2,-23,23,
-3,6,0,-3,-32,0,-10,-25,14,
-19,9,14,-27,20,15,-5,-27,18,
11,-6,24,7,-17,26,20,-31,-25,
-25,4,-16,30,33,23,-4,-4,23
]);

% This was the original problem
%data = array2d(1..rows, 1..cols,[
%33,30,-10,-6,18,7,-11,-23,6,
%-16,19,9,26,8,-19,8,-21,-14,
%-17,-12,-14,-31,30,13,13,19,16,
%6,11,1,-17,12,-4,7,14,-21,
%-18,31,34,22,-17,-19,-20,24,6,
%-33,18,17,15,-31,-5,-3,27,-3,
%18,20,-18,-31,-6,4,2,-12,24,
%-27,-14,4,29,3,5,29,8,-12,
%-15,-7,23,23,-9,8,6,-8,12,
%33,-23,19,-4,-8,7,11,12,-31,
%20,-19,-15,30,-11,32,-7,14,-5,
%23,-18,-32,2,31,-7,-8,24,16,
%-32,4,-10,14,6,-1,0,23,23,
%25,0,23,22,12,-28,-27,-15,-4,
%-30,-13,16,-3,-3,32,-3,-27,31,
%22,1,-26,4,-2,13,26,-17,-14,
%9,18,3,20,27,-32,11,27,13,
%17,-33,-7,-19,32,13,31,-2,-24,
%-31,27,31,-29,15,-2,29,15,-33,
%18,23,15,-28,0,30,4,12,-32,
%3,-34,27,25,18,26,-1,34,26,
%21,31,-10,13,30,-17,12,-26,31,
%23,-31,19,21,-17,10,2,23,-23,
%3,-6,0,3,32,0,10,-25,14,
%19,-9,14,27,-20,15,5,-27,18,
%11,-6,-24,7,-17,-26,20,31,25,
%-25,4,16,30,33,-23,-4,4,-23
%]);

output
[
  "total_sum: " ++ show(total_sum) ++ "\n" ++
  "row_sums: " ++ show(row_sums) ++ "\n" ++
  "col_sums: " ++ show(col_sums) ++ "\n" ++
  "row_signs: " ++ show(row_signs) ++ "\n" ++
  "col_signs: " ++ show(col_signs) ++ "\n" 
]
++
[
  if j = 1 then "\n" else " " endif ++ 
    show(data_x[i,j])
  | i in 1..rows, j in 1..cols
];