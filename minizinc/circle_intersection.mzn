%
% Non linear programming in Minizinc
% 
% From the ECLiPSe book, page 259
%
% The following ECLiPSe predicate was translated to Minizinc. 
% circle(X,Y):-
% 
%        4 $= X^2 + Y^2,
%        4 $= (X-1)^2 + (Y-1)^2,
%        squash([X,Y], 1e-5, lin).
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc


var float: X;
var float: Y;

solve satisfy;

constraint
   % X >= 0.0 /\ Y >= 0.0 /\
   4.0 = X*X + Y*Y /\
   4.0 = (X-1.0)*(X-1.0) + (Y-1.0)*(Y-1.0)

;

output
[
  "X: " ++ show(X) ++ "\n" ++
  "Y: " ++ show(Y) ++ "\n"
];