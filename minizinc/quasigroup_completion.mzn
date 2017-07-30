% 
% Quasigroup completion problem in MiniZinc.
% 
% See 
% Carla P. Gomes and David Shmoys:
% "Completing Quasigroups or Latin Squares: Structured Graph Coloring Problem"
%
  
% See also
% Ivars Peterson "Completing Latin Squares"
% http://www.maa.org/mathland/mathtrek_5_8_00.html
% """
% Using only the numbers 1, 2, 3, and 4, arrange four sets of these numbers into 
% a four-by-four array so that no column or row contains the same two numbers. 
% The result is known as a Latin square.
% ...
% The so-called quasigroup completion problem concerns a table that is correctly 
% but only partially filled in. The question is whether the remaining blanks in 
% the table can be filled in to obtain a complete Latin square (or a proper 
% quasigroup multiplication table).
% """
% 


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

% int: n = 5;
int: n = 4;
array[1..n, 1..n] of var 1..n: x;

predicate cp2d(array[int,int] of var int: x, array[int,int] of var int: y) =
  assert(index_set_1of2(x) = index_set_1of2(y) /\
         index_set_2of2(x) = index_set_2of2(y),
           "cp2d: x and y have different sizes",
     forall(i in index_set_1of2(x), j in index_set_2of2(x)) (
         y[i,j] = x[i,j]
    ) 
  )
; 


solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint
    % Example from Ruben Martins and Inès Lynce
    % Breaking Local Symmetries in Quasigroup Completion Problems, page 3
    % The solution is unique:
    % 1 3 2 5 4
    % 2 5 4 1 3
    % 4 1 3 2 5
    % 5 4 1 3 2
    % 3 2 5 4 1

    % cp2d(x,array2d(1..n, 1..n, 
    % [
    %  1, _, _, _, 4,
    %  _, 5, _, _, _,
    %  4, _, _, 2, _,
    %  _, 4, _, _, _,
    %  _, _, 5, _, 1
    % ]))

    % Example from Gomes & Shmoys, page 3.
    % Solution:
    % 4 1 2 3
    % 2 3 4 1
    % 1 4 3 2
    % 3 2 1 4
    % cp2d(x, array2d(1..n, 1..n, 
    % [
    %  _, 1, 2, 3,
    %  2, _, 4, 1, 
    %  1, 4, _, 2,
    %  3, _, 1, _
    %]))

    % Example from Gomes & Shmoys, page 7
    % Two solutions.
    cp2d(x, array2d(1..n, 1..n, 
    [
      _, 1, _, _,
      _, _, 2, _,
      _, 3, _, _,
      _, _, _, 4
    ]))

    /\ % make it a Latin Square
    forall(i in 1..n) (
      all_different([x[i,j] | j in 1..n])
      /\
      all_different([x[j,i] | j in 1..n])
    )

;

output [
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])  
  | i,j in 1..n
] ++ ["\n"];