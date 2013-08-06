% 
% Costas array in MiniZinc.
% 
% Note: This model is heavily based on Barry O'Sullivan's 
% MiniZinc model in the G12 repository:
% http://www.g12.cs.mu.oz.au/mzn/costas_array/CostasArray.mzn
%
%
% From http://mathworld.wolfram.com/CostasArray.html:
% """
% An order-n Costas array is a permutation on {1,...,n} such
% that the distances in each row of the triangular difference
% table are distinct. For example, the permutation {1,3,4,2,5}
% has triangular difference table {2,1,-2,3}, {3,-1,1}, {1,2},
% and {4}. Since each row contains no duplications, the permutation
% is therefore a Costas array.
% """
%
% Also see
% http://en.wikipedia.org/wiki/Costas_array
%

% About this model:
% As mentioned above this model is based on Barry O'Sullivan's 
% model. Here are the two rather simple differences 
% (marked by "hakank" below)
%  1) no symmetry breaking on the order of the Costas array
%  2) fixes the lower triangular matrix in the difference
%     matrix to -n+1
% 
% Since there is no symmetry breaking of the order of the Costas 
% array it gives all the solutions for a specific length of 
% the array, e.g. those 
% listed in http://mathworld.wolfram.com/CostasArray.html
% 
% 1	1	(1)
% 2	2	(1, 2), (2,1)
% 3	4	(1, 3, 2), (2, 1, 3), (2, 3, 1), (3, 1, 2)
% 4	12	(1, 2, 4, 3), (1, 3, 4, 2), (1, 4, 2, 3), (2, 1, 3, 4), 
%               (2, 3, 1, 4), (2, 4, 3, 1), (3, 1, 2, 4), (3, 2, 4, 1), 
%               (3, 4, 2, 1), (4, 1, 3, 2), (4, 2, 1, 3), (4, 3, 1, 2)
% ....
% 
% See http://www.research.att.com/~njas/sequences/A008404
% for the number of solutions for n=1..
% 1, 2, 4, 12, 40, 116, 200, 444, 760, 2160, 4368, 7852, 12828, 
% 17252, 19612, 21104, 18276, 15096, 10240, 6464, 3536, 2052, 
% 872, 200, 88, 56, 204,...
% 
%

% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 8;
array[1..n] of var 1..n: costas;
array[1..n,1..n] of var -n+1..n-1: differences;

% solve satisfy;
solve :: int_search(
        [differences[i,j] | i,j in 1..n] ++ costas,
        first_fail,
        indomain_min,
        complete)
    satisfy;

%
% hakank: Here are my two changes
%
% 1) I skipped this constraint since I want 
%    to generate all solutions.
% constraint costas[1] < costas[n];

% 2) Fix the values in the lower triangle in the
% difference matrix to -n+1. This removes variants 
% of the difference matrix for the the same Costas array.
constraint 
    forall(i in 1..n) (
        forall(j in 1..i) (
          differences[i,j] = -n+1
        )
    )
;

% hakank: All the following constraints are from 
% Barry O'Sullivans's original model.

constraint 
   all_different( costas );


% "How do the positions in the Costas array relate 
%  to the elements of the distance triangle."
constraint forall( i,j in 1..n where i < j)(
	differences[i,j] = costas[j] - costas[j-i]
	);
	

% "All entries in a particular row of the difference 
%  triangle must be distint."
constraint forall( i in 1..n-1 )(
	all_different(j in 1..n where j > i )(differences[i,j])
	);

% "All the following are redundant - only here to speed up search."

% "We can never place a 'token' in the same row as any other."
constraint forall(i,j in 1..n where i < j)(
	differences[i,j] != 0
	);

constraint forall(k,l in 3..n where k < l)(
	 differences[k-2,l-1] + differences[k,l] = 
	 differences[k-1,l-1] + differences[k-1,l]
	 );


output 
[
   % "costas: " ++ show(costas) ++ "\n"  
   show(costas) ++ ","  
]
++
[
  if j = 1 then "\n" else " " endif ++
     if fix(differences[i,j]) > -n+1 then 
         show(differences[i,j]) 
     else 
        " "
     endif
  | i,j in 1..n
]
++
["\n"];