% 
% Sum first and last in MiniZinc.
% 
% Requires MiniZinc 2.0.
%
% http://stackoverflow.com/questions/27434424/prolog-restrictions-solving-puzzle-grid
% """
% The goal of the puzzle is simple: I have a square grid with some numbers on 
% top/below each column and on the right/left of each row. The domain of values 
% goes from 0 to Gridsize -1, wich means, a grid 7x7 can have numbers from 0 to 6.
% The constraints are as follow:
% 
%   - Each number can only apear once each row and once each column
%   - The number on top/right are the sum of the First and Last digits on the 
%     column/row respectively
%   - The number on bottom/left are the sum of the Second and SecondLast digits 
%     on the column/row respectively
%   - Zeros don't count as digits, are only on the program to represent blank 
%     spaces
%
% For an example:
%
% TopConstraint = [7, 6, 4, 7, 3] 
% RightConstraint = [5, 5, 5, 5, 5] 
% Bottom Constraint = [3, 4, 6, 3, 7] 
% LeftConstraint = [5, 5, 5, 5, 5]
% 
% This constraints can have a 0 too, wich make the program simple ignore 
% (the sum can be any number, if it goes accordingly with the other restrictions).
%
% One solution to the above lists would be the matrix:
%
%  3 | 4 | 1 |   | 2 
%  1 | 3 | 2 | 4 |   
%  2 |   | 4 | 1 | 3 
%    | 1 | 3 | 2 | 4 
%  4 | 2 |   | 3 | 1
% """

% Note: This model give two (symmetric) solutions. The first is the same
% as the example, the second is the same as the first but with the order 
% of rows reversed.
%
%   3 4 1 0 2
%   1 3 2 4 0
%   2 0 4 1 3
%   0 1 3 2 4
%   4 2 0 3 1
%   ----------
%
%   4 2 0 3 1
%   0 1 3 2 4
%   2 0 4 1 3
%   1 3 2 4 0
%   3 4 1 0 2
%   ----------
%   ==========
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 5;

array[1..n] of var int: TopConstraint;
array[1..n] of var int: RightConstraint;
array[1..n] of var int: BottomConstraint;
array[1..n] of var int: LeftConstraint;

% decision variables
array[1..n, 1..n] of var 0..n-1: x;


% first element of a
function var int: first(array[int] of var int: a) = a[1];

% second element of a
function var int: second(array[int] of var int: a) = a[2];

% second last element of a
function var int: second_last(array[int] of var int: a) = a[length(a)-1];

% last element of a
function var int: last(array[int] of var int: a) = a[length(a)];


%
% Returns an array of the elements that are > 0.
% The num_zeros indicates the number of 0s in the array.
%
function array[int] of var int: except_0(array[int] of var int: a, int: num_zeros) =
    let {
       int: len = length(a),
       array[1..len-num_zeros] of var int: t;
       constraint
          forall(i in index_set(a)) (
             a[i] > 0 -> t[sum([a[j]>0 | j in 1..i-1])+1] = a[i]
          )
       ;
    } in t
;


% sum first and last numbers (except 0s)
function var int: sum_first(array[int] of var int: a) = 
    first(except_0(a,1)) + last(except_0(a,1))
;

% sum second and second last numbers (except 0s)
function var int: sum_second(array[int] of var int: a) = 
    second(except_0(a,1)) + second_last(except_0(a,1))
;


% solve satisfy;
solve :: int_search(array1d(x), first_fail, indomain_min, complete) satisfy;

constraint
   forall(i in 1..n) (
     let {
        array[int] of var 0..n-1: this_row    = [x[i,j] | j in 1..n],
        array[int] of var 0..n-1: this_column = [x[j,i] | j in 1..n]
     } in 
     all_different(this_row) /\
     all_different(this_column) 

     % Note: The given hints can be 0 and then there's no restriction
     /\ (TopConstraint[i]    > 0 -> TopConstraint[i]    = sum_first(this_column)  )
     /\ (RightConstraint[i]  > 0 -> RightConstraint[i]  = sum_first(this_row)     )

     /\ (LeftConstraint[i]   > 0 -> LeftConstraint[i]   = sum_second(this_row)    )
     /\ (BottomConstraint[i] > 0 -> BottomConstraint[i] = sum_second(this_column) )
    )
;


output [
   if j = 1 then "\n" else " " endif ++
    show(x[i,j])
   | i,j in 1..n
]
++
[
"\n", 
"TopConstraint   : ", show(TopConstraint), "\n",
"RightConstraint : ", show(RightConstraint), "\n",
"BottomConstraint: ", show(BottomConstraint), "\n",
"LeftConstraint  : ", show(LeftConstraint), "\n",

]
;


TopConstraint    = [7, 6, 4, 7, 3];
% TopConstraint    = [0, 0, 0, 0, 0]; % Testing
RightConstraint  = [5, 5, 5, 5, 5]; 
% RightConstraint  = [0, 0, 0, 0, 0]; % Testing
BottomConstraint = [3, 4, 6, 3, 7];
LeftConstraint   = [5, 5, 5, 5, 5];
% LeftConstraint   = [5, 5, 5, 5, 0]; % Testing
