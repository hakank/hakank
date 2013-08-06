% 
% Quasigroup problem in MiniZinc.
% 
% This model is a translation of the ESSENCE' model quasiGroup7.eprime
% from the Minion Translator examples. 
% """
% The quasiGroup existence problem (CSP lib problem 3)
%
% An m order quasigroup  is an mxm multiplication table of integers 1..m, 
% where each element occurrs exactly once in each row and column and certain 
% multiplication axioms hold (in this case, we want axiom 7 to hold). 
% """

% See
% http://www.dcs.st-and.ac.uk/~ianm/CSPLib/prob/prob003/index.html
% http://www.dcs.st-and.ac.uk/~ianm/CSPLib/prob/prob003/spec.html
% Axiom 7:
% """
% QG7.m problems are order m quasigroups for which (b*a)*b = a*(b*a).
% """
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: n; 
set of int: nDomain = 0..n-1;

array[nDomain, nDomain] of var nDomain: quasiGroup;

solve :: int_search([quasiGroup[row, col] | row, col in nDomain], 
        first_fail, indomain_min, complete) satisfy;
% solve satisfy;

constraint
     
     % assign the "reflected" quasigroup to qGColumns to access its columns
     %  forall row,col : nDomain .
     %    quasiGroupColumns[col,row] = quasiGroup[row,col],

     % All rows have to be different
     forall(row in nDomain) (
          all_different([quasiGroup[row,col] | col in nDomain])
     )
     /\
     % all values in the diagonals
     forall( i in nDomain ) (
          quasiGroup[i,i] = i
     )
     /\

     % All columns have to be different	       
     forall(col in nDomain) (
          all_different([quasiGroup[row, col] | row in nDomain])
     )
     /\

     % this strange constraint
     % corresponds to:
     % quasiGroup[i, quasiGroup[j,i]] = quasiGroup[quasiGroup[j,i], j]	
     forall(i,j in nDomain) (
              quasiGroup[i, quasiGroup[j,i]] = quasiGroup[quasiGroup[j,i],j]
     )
     /\
     % some implied? constraint
     forall(i in nDomain) (
           quasiGroup[i,n-1] + 2 >= i
     )
;

output [
  if col = 0 then "\n" else " " endif ++
    show(quasiGroup[row, col])
  | row, col in nDomain
] ++ ["\n"];


%
% data
%
n = 5;