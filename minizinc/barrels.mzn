% 
% Barrels in MiniZinc.
% 
% From September Magic Contest 2014
% by Riad Khanmagomedo
% http://logicmastersindia.com/lmitests/dl.asp?attachmentid=468 
% (SeptemberMagicContest2014_lns.pdf)
% """
% Fill the white cells on the each barrels side with different digits from 1 to 6. 
% Digits cannot repeat in every horizontal and vertical directions. Each number 
% on the barrels top must be equal to the sum or product of the four different 
% digits in the barrel. All top numbers are different and less than 91.
%
% [
%    
%   Barrels:
%
%      -      -     - 
%    ___    ___   ___
%    X 3    X X   6 X
%    X X    X X   X X
% 
%    13       -   30
%    ___    ___   ___
%    X X    X 4   X X
%    X X    5 X   X X
% 
%    15     24    -
%    ___    ___   ___
%    X X    X 2   5 X
%    X 1    X X   X X
% 
% ]
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 


int: n = 9; % 9 barrels
int: m = 2; % each barrel contens is 2x2
int: t = 6; % the 6x6 matrix


% decision variables
array[1..n] of var 1..91: barrels = 
    [_, _,  _, 
    13, _, 30, 
    15, 24, _];

array[1..2] of string: ops = ["prod","sum"];
array[1..n] of var 1..2: barrels_op;

array[1..n, 1..m, 1..m] of var 1..t: contents = 
  array3d(1..n, 1..m, 1..m,
  [
    _, 3, _, _, % barrel 1     1  2  7  8
    _, _, _, _, % barrel 2     3  4  9 10
    6, _, _, _, % ..           5  6 11 12

    _, _, _, _, %             13 14 19 20
    _, 4, 5, _, %             15 16 21 22
    _, _, _, _, %             17 18 23 24

    _, _, _, 1, %             24 25 31 32
    _, 2, _, _, %             27 28 33 34
    5, _, _, _, % barrel 9    29 30 35 36
  ]);

array[1..t*t] of var 1..t: contents_flat = array1d(1..t*t, contents);

%
% 6x6 representation of contents
%
array[1..t,1..t] of var 1..t: mat;

% convert indices of contents to indices in mat
array[1..t,1..t] of int: mat_ix = 
array2d(1..t,1..t,
[
   1, 2,   5, 6,   9,10,   % 1 1  2 2  3 3 
   3, 4,   7, 8,  11,12,   % 1 1  2 2  3 3 

  13,14,  17,18,  21,22,   % 4 4  5 5  6 6
  15,16,  19,20,  23,24,   % 4 4  5 5  6 6 

  25,26,  29,30,  33,34,   % 7 7  8 8  9 9
  27,28,  31,32,  35,36,   % 7 7  8 8  9 9
  
]);


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  all_different(barrels) 
  /\
  forall(b in 1..n) (
     let {
       array[1..m*2] of var 1..t: tmp = [contents[b,i,j] | i,j in 1..m]
     } in
     all_different(tmp) 
     /\
     (
       (product(tmp) = barrels[b] /\ barrels_op[b] = 1)
       \/
       (sum(tmp) = barrels[b]  /\ barrels_op[b] = 2)
     )
  )
  /\ % convert mat <-> contents
  forall(i,j in 1..t) (
    mat[i,j] = contents_flat[mat_ix[i,j]]
  )
  /\ % mat is a latin square
  forall(i in 1..t) (
    all_different([mat[i,j] | j in 1..t]) 
    /\
    all_different([mat[j,i] | j in 1..t]) 
  )

;

output [
  "barrels: ", show(barrels), "\n"
]
++
[
 "barrel " ++ show(b) ++ " (" ++ show_int(2,barrels[b]) ++ "): " ++
  show([contents[b,i,j] | i,j in 1..m])  ++ " op: " ++ show(ops[fix(barrels_op[b])]) ++ "\n"
  | b in 1..n
]
++
[
 "barrels grid:"
]
++
[
  if j = 1 then "\n" else " " endif ++
  if j = 1 /\ i mod 2 = 1 then "\n" else "" endif ++
  if j mod 2 = 1 then "  " else "" endif ++
     show(mat[i,j])
  | i,j in 1..t
]
;

