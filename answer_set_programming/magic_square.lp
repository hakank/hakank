% 
% Magic square in ASP.
%
% See http://en.wikipedia.org/wiki/Magic_square
%
% 
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

#const n = 3.               % the size
#const s = n*(n*n + 1) / 2. % the num

%
% domains
%
size(1..n).
val(1..n*n).

% unique index of x
1 { x(Row, Col, N) : val(N) } 1 :- size(Row;Col).

% alldifferent values of x
1 { x(Row, Col, N) : size(Row;Col) } 1 :- val(N).


% sum rows
:- not s #sum[ x(Row, Col, Val) : size(Col) : val(Val) = Val ] s, size(Row).

% sum columns
:- not s #sum[ x(Row, Col, Val) : size(Row) : val(Val) = Val ] s, size(Col).

% Sum diagonals 1 and 2
:- not s #sum[ x(I, I, Val) : size(I) : val(Val) = Val ] s.
:- not s #sum[ x(I, n-I+1, Val) : size(I) : val(Val) = Val ] s.

#hide.
#show x(Row, Col, N).