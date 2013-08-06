% 
% Seating plan problem in MiniZinc.
% 
% From Daniel L. Dudley (The Nutcracker Suite)
% http://home.chello.no/~dudley/
% """
% *  Seating plan    (Nut # 1)
%
% A university was to hold an examination in 5 subjects: 
% Norwegian, German, English, French and Spanish. In each of 
% these languages there were 4 candidates. The examination room 
% was therefore divided into 20 cells, as shown in the figure 
%  below:
%
%           X
%           X X X X X
%           X X X X
%           X X X X
%         X X X X X
%                 X
%
% The university's administration wanted to secure themselves 
% against cheating. Candidates in the same language were to be 
% completely isolated from each other, so much so that their cells 
% were not to coincide even at the corners.
%
% A young lecturer was given the job of finding a solution to the 
% problem, which he did—and justly received a pat on the back from 
% the dean. Can you earn his acclamation, too?
%
% Now it just so happens that the dean is an ardent Prolog programmer 
% in his spare time (how else could he make dean?) and, realizing 
% that there could be several solutions to the problem, used his skills 
% to find all solutions. Can you do the same (in the programming 
% language of your choice)?  Note: "several solutions" is a 
% gross understatement!
%
% Source: Daniel L. Dudley
%
% """
%
% Note: There are 3240 solutions.
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 6;
array[1..n, 1..n] of var 0..5: x;
array[1..n, 1..n] of 0..1: data;

predicate all_different_except_0(array[int] of var int: x) =
   forall(i,j in index_set(x) where i != j) (
        (x[i] != 0 /\ x[j] != 0) -> x[i] != x[j]      
   )
;


% solve satisfy;
solve :: int_search([x[i,j] | i,j in 1..n], first_fail, indomain_min, complete) satisfy;

constraint

    forall(i,j in 1..n) (
      if data[i,j] = 0 then
         x[i,j] = 0
      else 
         x[i,j] > 0
      endif
    )
    /\
    forall(i in 1..n) (
      all_different([x[i,j] | j in 1..n where data[i,j] = 1])
      /\
      all_different([x[j,i] | j in 1..n where data[j,i] = 1])

      % a variant:
      % all_different_except_0([x[i,j] | j in 1..n])
      % /\
      % all_different_except_0([x[j,i] | j in 1..n])
    )
    /\ % check corners
    forall(i,j in 2..n-1 where i < j) (
       if data[i,j] > 0 then
          forall(a,b in {-1,0,1} where not( a=0 /\ b=0)) (
              x[i+a,j+b] != x[i,j]
          )
       else 
         true
       endif
    )
    /\ % ensure that there are 4 of each subject
    global_cardinality([x[i,j] | i,j in 1..n where data[i,j] = 1], set2array(index_set([4,4,4,4,4])), [4,4,4,4,4])
    
;


%
% data:
% 0 -> no seat
% 1 -> 1..5
%
data = array2d(1..n,1..n,
        [0,1,0,0,0,0,
         0,1,1,1,1,1,
         0,1,1,1,1,0,
         0,1,1,1,1,0,
         1,1,1,1,1,0,
         0,0,0,0,1,0
       ]);

output[

  if j = 1 then "\n" else " " endif ++
    if fix(x[i,j]) > 0 then show(x[i,j]) else  " " endif
  | i, j in 1..n

]
++
["\n"];

