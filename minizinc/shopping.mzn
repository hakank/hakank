% 
% Shopping puzzle in MiniZinc.
%
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles4.html, puzzle nr. 10.
% Source:  M Kraitchik, Mathematical Recreations(p37), Dover
%
% This model was inspired by the AMPL model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol4s10.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: m = 3;
set of int: M = 1..m;

% Husbands:Arthur, Bernard, Charles 
% Wifes   : Ann, Barbara, Cynthia 
array[M, M] of var 0..1: w; % w[i,j]=1 if husband i married to wife j, 0 otherwise 
array[M] of var 1..100: x; % number of articles bought by husband i
array[M] of var 1..100: y; % number of books (and price) bought by son j

solve satisfy;
% solve :: int_search([w[i,j] | i,j in M], "first_fail", "indomain", "complete") satisfy;

constraint
   % Each husband has one wife
   forall(i in M) (
      sum (j in M) (w[i,j]) = 1
   )

   /\ % Each wife has one husband
   forall(j in M) (
     sum(i in M) (w[i,j]) = 1
   )

   /\ % Arthur has bought 23 more articles than Barbara
   x[1] = y[2] + 23
 
   /\ % Bernard has bought 11 more articles than Ann
   x[2] = y[1] + 11
   
   /\ % each husband has spent $63 more than his wife
   forall(i in M) (
       x[i]*x[i]  = sum(j in M) ( w[i,j]*y[j]*y[j]+63)
  )
;


output 
[
  "x: ", show(x), "\n",
  "y: ", show(y), 
]
++
[
  if j = 1 then "\n" else " " endif ++
    show(w[i,j])
  | i,j in M
] ++ ["\n"];