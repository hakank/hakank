% 
% Quasigroup problem in MiniZinc.
% 
% This model is a translation of the ESSENCE' model quasiGroup3NonIdempotent.eprime
% from the Minion Translator examples. 
% """
% The quasiGroup existence problem (CSP lib problem 3)
%
% An m order quasigroup  is an mxm multiplication table of integers 1..m, 
% where each element occurrs exactly once in each row and column and certain 
% multiplication axioms hold (in this case, we want axiom 3 to hold). 
% """
% 
% http://www.dcs.st-and.ac.uk/~ianm/CSPLib/prob/prob003/spec.html:
%

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: n = 4;
set of int: nDomain = 0..n-1;

array[nDomain, nDomain] of var nDomain: quasiGroup;
array[nDomain] of var nDomain: qgDiagonal;

% solve satisfy;
solve :: int_search([quasiGroup[row, col] | row, col in nDomain], first_fail, indomain_min, complete) satisfy;
% solve :: int_search(qgDiagonal, first_fail, indomain_min, complete) satisfy;

constraint
     % accessor for diagonal
     forall(i in nDomain) (
         qgDiagonal[i] = quasiGroup[i,i] 
     )
     /\
     % All rows have to be different
     forall(row in nDomain) (
          all_different([quasiGroup[row,col] | col in nDomain])
     )
     /\
     % All columns have to be different	       
     forall(col in nDomain) (
          all_different([quasiGroup[row,col] | row in nDomain])
     )
     /\
     %  (j*i)*(i*j) = i
     forall(i in nDomain) (
         forall(j in nDomain) (
	    quasiGroup[quasiGroup[i,j],quasiGroup[j,i]] = i
         )
     )
     /\
     % Implied (from Colton,Miguel 01)
     % All-diff diagonal
     all_different(qgDiagonal)

     /\

     % anti-Abelian
     forall(i in nDomain) (
       forall(j in nDomain) (
         (i != j) ->
         (quasiGroup[i,j] != quasiGroup[j,i])
       )
     )
     /\

     % if (i*i)=j then (j*j) = i
     forall(i in nDomain) (
       forall(j in nDomain) (
         (quasiGroup[i,i]=j) -> (quasiGroup[j,j]=i)
       )
     )
     /\

     % Symmetry-breaking constraints	
     forall(i in nDomain) (
           quasiGroup[i,n-1] + 2 >= i
     )
;

output 
[ "\nqgDiagonal: ", show(qgDiagonal)
] ++
[ "\nquasiGroup: "
] ++
[
  if col = 0 then "\n" else " " endif ++
    show(quasiGroup[row, col])
  | row, col in nDomain
] ++ ["\n"];




