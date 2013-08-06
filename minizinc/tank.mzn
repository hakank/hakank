% 
% Tank Attack Puzzle in MiniZink
% 
% 
% From http://www.informs.org/site/ITE/article.php?id=70
% "Martin J Chlond Puzzle - A Tank Attack Puzzle"
% Informs Transations on Educations, Volume 8, May 2008
% PDF: http://www.informs.org/site/ITE/downloadfile.php?i=c0c7c76d30bd3dcaefc96f40275bdc0a
% 

%
% This model was inspired by the original MathProg model (integer programming), 
% with the following
% notice:
% model name     : tank.mod
% description    : tank puzzle (courtesy of Adam Atkinson G4G7)
% written by     : Martin Chlond
% date written   : 6/2/08
%

% 
% This Minizinc program was written by Hakan Kjellerstrand, 
% hakank@bonetmail.com,
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: n = 6;

% indices
set of int: N = 1..n;
set of int: M = 1..n-1;

% variables
array[N, N, M] of var 0..1: x; % x[i,j,k] = 1 if number on tank {i,j} = k
array[N, N] of var int: res; % the output result

solve :: int_search([x[i,j,k] | i, j in N, k in M], first_fail, indomain_min, complete) satisfy;
% solve :: int_search([res[i,j] | i, j in N], first_fail, indomain_min, complete) satisfy;

constraint

  % attacks on tank = number on tank
  forall(i in N,j in N) (
    (
     sum(p in N where p != j) ( x[i,p,abs(j-p)]) + 
     sum(q in N where q != i) (x[q,j,abs(i-q)])
    ) = sum(k in M) (k*x[i,j,k])
  )

  /\  % one number on each tank
  forall(i in N,j in N) (
    sum(k in M) (x[i,j,k]) = 1
  )

  /\ % symmetry constraints - otherwise too slow
  forall(i in N,j in N,k in M) (
      x[i,j,k] = x[(n+1-i),(n+1-j),k]
  )
  /\
  forall(i in N,j in N,k in M) (
    x[i,j,k] = x[j,(n+1-i),k]
  )

  /\ % for the output
  forall(i, j in N) (
     res[i,j] = sum(k in 1..n-1) (k*x[i,j,k])
  )
;

output [
   if j = 1 then "\n" else " " endif ++
     show(res[i,j])
   | i,j in N
] ++ ["\n"];
