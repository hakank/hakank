%
% Global constraint sum_free in Minizinc
%
%
% From the Global Constraint Catalog
% http://www.emn.fr/x-info/sdemasse/gccat/Csum_free.html
% sum_free(S)
% """
% Impose for all pairs of values (not necessarily distinct) i, j of the 
% set S the fact that the sum i + j in not an element of S.
% """
%  
% Example: S = {1,3,5,9}
% Since  1+1 = 2 not in S
%        1+3 = 4 not in S
%        3+5 = 8 not in S
%        ...
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn";

% implements as an array, not a set...
array[1..10] of var 1..20: x = [_,_,_,2,3,_,_,_, _, _];

predicate sum_free(array[int] of var int: x) =
   forall(i,j,k in index_set(x))  (
      x[k] != x[i] +x[j]                  
   ) 
;

predicate product_free(array[int] of var int: x) =
   forall(i,j,k in index_set(x))  (
     x[k] != x[i] * x[j]                  
   ) 
;

% solve maximize sum(i in index_set(x)) (x[i]);
solve satisfy;

% adds some extra constraints just for fun
constraint 
   sum_free(x) 
   /\
   product_free(x) 
   /\ 
   forall(i in 1..100) (
     at_most(3, x, i)
   )
   /\ sum(i in index_set(x)) (x[i]) = 100
    % /\ all_different(x)
;


output [
     show(x), "\n"
];
