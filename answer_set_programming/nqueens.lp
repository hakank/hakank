% 
% N-queens in ASP.
% 
% See http://en.wikipedia.org/wiki/Eight_queens_puzzle
% 
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

#const n = 8.

% domain
number(1..n).

% alldifferent
1 { q(X,Y) : number(Y) } 1 :- number(X).
1 { q(X,Y) : number(X) } 1 :- number(Y).

% remove conflicting answers
:- q(X1,Y1), q(X2,Y2), X1 < X2, Y1 == Y2.
:- q(X1,Y1), q(X2,Y2), X1 < X2, Y1 + X1 == Y2 + X2.
:- q(X1,Y1), q(X2,Y2), X1 < X2, abs(Y1 - X1) == abs(Y2 - X2).


#hide.
#show q(I,J).
