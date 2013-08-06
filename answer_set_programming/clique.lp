% 
% Clique in ASP.
% 
% This was inspired by the encoding of clique in
% From http://en.wikipedia.org/wiki/Answer_set_programming#Large_clique
%
% 
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

% The problems are from
% http://www.hakank.org/minizinc/clique.mzn

v(X) :- e(X,Y).
v(Y) :- e(X,Y).

n {in(X) : v(X)}.
% n {in(X) : v(X)} n. % exact size of clique

% Clique criteria
:- in(X), in(Y), v(X), v(Y), X!=Y, not e(X,Y), not e(Y,X).

% Find maximum clique
%#maximize [in(X) : v(X)].

#hide.
#show in(X).