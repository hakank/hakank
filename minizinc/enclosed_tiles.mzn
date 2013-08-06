% 
% Enclosed tiles in MiniZinc.
% 
% From Stack Overflow
% "How do I write a program to find the tiles that are completely enclosed?"
% http://stackoverflow.com/questions/10067802/how-do-i-write-a-program-to-find-the-tiles-that-are-completely-enclosed
% """
% I am trying to write a prolog program that can find the tiles that are 
% completely surrounded by b or w in a 2D-array.
%
% For example, given a dataset like this:
%
% [
%     [b, w, +, +],
%     [w, +, w, b],
%     [+, w, b, +],
%     [+, +, +, b],
% ]
%
% It would return another Variable containing:
%
% [
%     [-, -, -, -],
%     [-, w, -, -],
%     [-, -, -, b],
%     [-, -, -, -],
% ]
%
% That is, it replaced all the + which were completely surrounded with b with a b, 
% and the same for those surrounded by a w, and replaced everything else with a -.
% 
% Can anyone give any ideas on how to build a program to do this?
%
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
int: rows = 4;
int: cols = 4;

int: M = 1; % -
int: P = 2; % +
int: B = 3; % b
int: W = 4; % w

array[1..rows, 1..cols] of int: data;
array[1..4] of string: str = ["-","+","b","w"];

% decision variables
array[1..rows, 1..cols] of var {M,B,W}: x;



%
% All elements in array a are equal to value v
%
predicate all_equal_constant(var int: v, array[int] of var int: a) =
      forall(i in index_set(a)) (
         a[i] = v
      )
;


%
% collect all the neighbours and check if all are
% of value t
%
predicate check(array[int,int] of int: d,
                int: i, int: j,
                int: t) =
        all_equal_constant(t, 
                           [d[i+a, j+b] | a, b in {-1,0,1} 
                                          where
                                          i+a >= 1 /\ i+a <= rows /\
                                          j+b >= 1 /\ j+b <= cols /\ 
                                          abs(a)+abs(b)=1])
;


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint

   forall(i in 1..rows, j in 1..cols) (
        ( 
         % if data[i,j] = "+"
         data[i,j] = P ->
         (
          (check(data, i, j, W) <-> x[i,j] = W)
          /\
          (check(data, i, j, B) <-> x[i,j] = B)
         )
        )
        /\
        ( % not a "+"
          data[i,j] != P -> x[i,j] = M
        )
   )
;

output 
[
  if j = 1 then "\n" else " " endif ++
    show(data[i,j])
  | i in 1..rows, j in 1..cols
]
++
["\n"] 
++
[
  if j = 1 then "\n" else " " endif ++
    show(str[data[i,j]])
  | i in 1..rows, j in 1..cols
]
++
["\nx:"] 
++ 
[
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i in 1..rows, j in 1..cols
]
++
["\nx:"] 
++ [
  if j = 1 then "\n" else " " endif ++
    show(str[fix(x[i,j])])
  | i in 1..rows, j in 1..cols
]


 ++ ["\n"]
;

%
% Data
%
% Original problem:
%
%  [b, w, +, +,
%   w, +, w, b,
%   +, w, b, +,
%   +, +, +, b]
%
% Solution:
% - - - -
% - w - -
% - - - b
% - - - -


data = array2d(1..rows, 1..cols, 
[
   B, W, P, P,
   W, P, W, B,
   P, W, B, P,
   P, P, P, B
]);


% Testing: changing (1,1) to "+" and 1,4 to "w"
%  [+, w, +, +,
%   w, +, w, b,
%   +, w, b, +,
%   +, +, +, b]

% Solution:
% w - w -
% - w - -
% - - - b
% - - - -

% data = array2d(1..rows, 1..cols, 
% [
%    P, W, P, W,
%    W, P, W, B,
%    P, W, B, P,
%    P, P, P, B
% ]);
