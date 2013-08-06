%
% Just forgotten puzzle (Enigma 1517) in ASP.
% 
% From http://www.f1compiler.com/samples/Enigma%201517.f1.html
% """
%
% Enigma 1517 Bob Walker, New Scientist magazine, October 25, 2008.
%
% 
% Joe was furious when he forgot one of his bank account numbers. 
% He remembered that it had all the digits 0 to 9 in some order, so he tried
% the following four sets without success:
%
%     9 4 6 2 1 5 7 8 3 0
%     8 6 0 4 3 9 1 2 5 7 
%     1 6 4 0 2 9 7 8 5 3
%     6 8 2 4 3 1 9 0 7 5
%
% When Joe finally remembered his account number, he realised that in each set
% just four of the digits were in their correct position and that, if one knew
% that, it was possible to work out his account number.
% What was it?
%
% """
% 
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%


% Solutions:
% The number is 9 6 2 4 3 1 7 8 5 0

#const r = 4.
#const c = 10.

% domains
rows(1..r).
cols(1..c).
number(0..9).

%
% 9,4,6,2,1,5,7,8,3,0
% 8,6,0,4,3,9,1,2,5,7
% 1,6,4,0,2,9,7,8,5,3
% 6,8,2,4,3,1,9,0,7,5
%
a(1, 1, 9).
a(1, 2, 4).
a(1, 3, 6).
a(1, 4, 2).
a(1, 5, 1).
a(1, 6, 5).
a(1, 7, 7).
a(1, 8, 8).
a(1, 9, 3).
a(1, 10, 0).

a(2, 1, 8).
a(2, 2, 6).
a(2, 3, 0).
a(2, 4, 4).
a(2, 5, 3).
a(2, 6, 9).
a(2, 7, 1).
a(2, 8, 2).
a(2, 9, 5).
a(2, 10, 7).

a(3, 1, 1).
a(3, 2, 6).
a(3, 3, 4).
a(3, 4, 0).
a(3, 5, 2).
a(3, 6, 9).
a(3, 7, 7).
a(3, 8, 8).
a(3, 9, 5).
a(3, 10, 3).

a(4, 1, 6).
a(4, 2, 8).
a(4, 3, 2).
a(4, 4, 4).
a(4, 5, 3).
a(4, 6, 1).
a(4, 7, 9).
a(4, 8, 0).
a(4, 9, 7).
a(4, 10, 5).

%
% alldifferent values of x
% 
1 { x(I, Val) : number(Val) } 1 :- cols(I).
1 { x(I, Val) : cols(I)  } 1 :- number(Val).

%
% For all 4 sets there there are exactly 4 numbers
% that are correct.
%
4 { x(Col, Val) : a(Row, Col, Val2) : number(Val;Val2) : Val == Val2 } 4 :- rows(Row).


#hide.
#show x(Col, Val).