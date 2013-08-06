%
% Assignmen problem in ASP.
% 
% Winston "Operations Research", Assignment Problems, page 393f
%
% This is a generalized version with an added test column.
%
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

#const r = 4.
#const c = 5.

%
% cost matrix:
% 
% 14,  5,  8,  7, 15,
%  2, 12,  6,  5,  3,
%  7,  8,  3,  9,  7,
%  2,  4,  6, 10,  1
%
cost(1, 1, 14).
cost(1, 2, 5).
cost(1, 3, 8).
cost(1, 4, 7).
cost(1, 5, 15).
cost(2, 1, 2).
cost(2, 2, 12).
cost(2, 3, 6).
cost(2, 4, 5).
cost(2, 5, 3).
cost(3, 1, 7).
cost(3, 2, 8).
cost(3, 3, 3).
cost(3, 4, 9).
cost(3, 5, 7).
cost(4, 1, 2).
cost(4, 2, 4).
cost(4, 3, 6).
cost(4, 4, 10).
cost(4, 5, 1).

% domains:
rows(1..r).
cols(1..c).

% 1: assigned, 0: not assigned
values(0..1).

%
% exacly one assignment per row, 
% all rows must be assigned
%
1 { x(Row, Col, 1) : cols(Col)  } 1 :- rows(Row).


% zero or one assignments per column
0 { x(Row, Col, 1) : rows(Row) } c :- cols(Col).
% :- 2 { x(Row, Col, 1) : rows(Row) }, cols(Col).

#minimize [ x(Row, Col, Val) : cost(Row, Col, Cost) = Val*Cost ].

#hide.
#show x(Row, Col, Val).