% 
% Futoshiki problem in MiniZinc.
% 
% http://en.wikipedia.org/wiki/Futoshiki
% http://www.guardian.co.uk/world/2006/sep/30/japan.estheraddley
%
%
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

number(1..size).

x(X,Y,N) :- hint(X,Y,N).

%
% Latin square: all rows and columns must be different.
% 
1 { x(X,Y,N):number(N) } 1 :- number(X;Y).
1 { x(X,Y,N):number(X) } 1 :- number(N;Y).
1 { x(X,Y,N):number(Y) } 1 :- number(N;X). 

% all < constraints must be satisfied

:- x(X1,Y1,N1), x(X2,Y2,N2), less_than(X1,Y1, X2,Y2), N1 >= N2.