% 
% Special 6 digit number in MiniZinc.
% 
% From 
% Presh Talwalkar (Mind Your Decisions) 
% https://www.youtube.com/watch?v=KXOjtmNUSH0&feature=youtu.be
% """
% A 6 digit number abcdef has the following property. Make a table with 6 rows, 
% where row 2 is the number abcdef multiplied by 2, and row i is the number abcdef multiplied by i 
% for values i = 3, 4, 5, 6. 
% (If the first row was 123456, the second row would be 123456 x 2 = 246912). 
% 
% For this number abcdef, the table is a Latin square–each row has 6 different digits with 
% no repeats, and each column also has those 6 different digits with no repeats.
%
% Can you figure out a value of abcdef? 
% """

% Solution:
% y: [142857, 285714, 428571, 571428, 714285, 857142]

% 1 4 2 8 5 7
% 2 8 5 7 1 4
% 4 2 8 5 7 1
% 5 7 1 4 2 8
% 7 1 4 2 8 5
% 8 5 7 1 4 2
% ----------
% ==========
%
% 
% This is the "magic" 1/7 series: 
% Picat> println([to_fstring("\n%0.6f", I*1/7) : I in 1..6])  
% [
% 0.142857,
% 0.285714,
% 0.428571,
% 0.571429,
% 0.714286,
% 0.857143]

% Number of solutions for different n:
% 
%  n   # solutions
% ----------------
%  (0    1)
%  1   10
%  2   37
%  3   84
%  4   96
%  5   21   
%  6    1
%  7    0
% 
% The sequence is not found in OEIS: [10,37,84,96,21,1] .

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n=6;

% decision variables
array[1..n, 1..n] of var 0..9: x;      % the matrix
array[1..n] of var 0..pow(10, n)-1: y; % the numbers

% convert the number n to/from the array a base base
% n = to_num_base(a, base)
function var int: to_num_base(array[int] of var int: a, int: base) =
          let { int: len = card(index_set(a));
                var int: n = sum(i in index_set(a)) (
                   pow(base, len-i) * a[i] 
                 );
         } in n
;

% base 10, as a function
function var int: to_num(array[int] of var int: a) = to_num_base(a, 10);

solve satisfy;

constraint
  forall(i in 1..n) (
    % convert row to a number 
    y[i] = to_num([x[i,j] | j in 1..n]) /\ 
    y[i] = i*y[1]                       /\ 

    % latin square
    all_different([x[i,j] | j in 1..n]) /\ 
    all_different([x[j,i] | j in 1..n])
  )
;

output 
[
  "y: \(y)\n",
]
++
[
  if j = 1 then "\n" else " " endif ++
    show(x[i,j]) 
  | i, j in 1..n
]
% ++ 
% ["\ni/7:\n"]
% ++
% [
%   show_float(2,7, i/7) ++ "\n"
%   | i in 1..n
% ]
;

