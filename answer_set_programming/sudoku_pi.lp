% 
% Pi Day Sudoku (2009) problem in ASP.
% 
% "Pi Day 2009"
% http://brainfreezepuzzles.com/main/piday2009.html
%
% Via 360 "Pi Day Sudoku 2009"
% http://threesixty360.wordpress.com/2009/03/09/pi-day-sudoku-2009/
% """
% Each row, column, and region contains the digits 1-9 exactly once plus 
% three π [Pi] symbols.  There’s a printable .pdf file 
% here [http://brainfreezepuzzles.com/main/files/Brainfreeze_PiDay2009.pdf]
%
% As a bonus, if you send a correct solution in to Brainfreeze puzzles in 
% the next couple months, you're eligible for a drawing for their book 
% on Color Sudoku!  More details are on their website [[http://brainfreezepuzzles.com/]].
% """

% 
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

% Here is the problem:
% P: pi
%
%  4 9 7  P 5 _ _ _ _  _ _ _ 
%  _ P _  8 _ _ 9 6 1  5 2 _ 
%  _ 8 _  1 _ _ _ P _  7 _ _ 
%  _ _ _  _ _ _ _ P _  4 _ _ 
%  5 3 9  6 _ _ _ _ _  _ _ _ 
%
%  9 4 _  P P P 7 _ _  _ _ _ 
%  _ _ _  _ _ 6 2 5 P  _ 7 4 
%  _ _ _  _ _ _ _ _ P  P 3 8 
%  _ 7 8  4 6 9 _ _ _  _ _ _ 
%
%  _ _ 3  _ P _ _ 4 7  1 6 9 
%  _ _ 4  _ 1 _ _ _ 6  _ P _ 
%  _ _ _  _ _ _ _ _ 4  _ 5 _
%

% The solution:
%
%  4 9 7  P 5 2 P 3 P  6 8 1 
%  P P P  8 4 7 9 6 1  5 2 3 
%  2 8 P  1 3 5 6 P 9  7 4 P 
%  1 6 P  7 2 8 3 P 5  4 9 P 
%
%  5 3 9  6 P 4 1 2 8  P P 7 
%  9 4 5  P P P 7 8 3  2 1 6 
%  P P 1  3 8 6 2 5 P  9 7 4 
%  6 P 2  5 7 1 4 9 P  P 3 8 
%
%  3 7 8  4 6 9 P 1 2  P P 5 
%  P 5 3  2 P P 8 4 7  1 6 9 
%  7 2 4  9 1 3 5 P 6  8 P P 
%  8 1 6  P 9 P P 7 4  3 5 2 
%

hint(1, 1, 4).
hint(1, 2, 9).
hint(1, 3, 7).
hint(1, 4, p).
hint(1, 5, 5).
hint(2, 2, p).
hint(2, 4, 8).
hint(2, 7, 9).
hint(2, 8, 6).
hint(2, 9, 1).
hint(2, 10, 5).
hint(2, 11, 2).
hint(3, 2, 8).
hint(3, 4, 1).
hint(3, 8, p).
hint(3, 10, 7).
hint(4, 8, p).
hint(4, 10, 4).
hint(5, 1, 5).
hint(5, 2, 3).
hint(5, 3, 9).
hint(5, 4, 6).
hint(6, 1, 9).
hint(6, 2, 4).
hint(6, 4, p).
hint(6, 5, p).
hint(6, 6, p).
hint(6, 7, 7).
hint(7, 6, 6).
hint(7, 7, 2).
hint(7, 8, 5).
hint(7, 9, p).
hint(7, 11, 7).
hint(7, 12, 4).
hint(8, 9, p).
hint(8, 10, p).
hint(8, 11, 3).
hint(8, 12, 8).
hint(9, 2, 7).
hint(9, 3, 8).
hint(9, 4, 4).
hint(9, 5, 6).
hint(9, 6, 9).
hint(10, 3, 3).
hint(10, 5, p).
hint(10, 8, 4).
hint(10, 9, 7).
hint(10, 10, 1).
hint(10, 11, 6).
hint(10, 12, 9).
hint(11, 3, 4).
hint(11, 5, 1).
hint(11, 9, 6).
hint(11, 11, p).
hint(12, 9, 4).
hint(12, 11, 5).


%
% the 12 regions
%
% Upper left dark green
region(1, 1,1).
region(1, 1,2).
region(1, 1,3).
region(1, 2,1).
region(1, 2,2).
region(1, 2,3).
region(1, 3,1).
region(1, 3,2).
region(1, 4,1).
region(1, 4,2).
region(1, 5,1).
region(1, 5,2).

% Upper mid light dark green).
region(2, 1,4).
region(2, 1,5).
region(2, 1,6).
region(2, 1,7).
region(2, 1,8).
region(2, 1,9).
region(2, 2,4).
region(2, 2,5).
region(2, 2,6).
region(2, 2,7).
region(2, 2,8).
region(2, 2,9).

% Upper right green
region(3, 1,10).
region(3, 1,11).
region(3, 1,12).
region(3, 2,10).
region(3, 2,11).
region(3, 2,12).
region(3, 3,11).
region(3, 3,12).
region(3, 4,11).
region(3, 4,12).
region(3, 5,11).
region(3, 5,12).


% Mid upper left "blue"
region(4, 3,3).
region(4, 3,4).
region(4, 3,5).
region(4, 3,6).
region(4, 4,3).
region(4, 4,4).
region(4, 4,5).
region(4, 4,6).
region(4, 5,3).
region(4, 5,4).
region(4, 5,5).
region(4, 5,6).


% Mid Upper right blue
region(5, 3,7).
region(5, 3,8).
region(5, 3,9).
region(5, 3,10).
region(5, 4,7).
region(5, 4,8).
region(5, 4,9).
region(5, 4,10).
region(5, 5,7).
region(5, 5,8).
region(5, 5,9).
region(5, 5,10).


% Mid left green
region(6, 6,1).
region(6, 6,2).
region(6, 6,3).
region(6, 7,1).
region(6, 7,2).
region(6, 7,3).
region(6, 8,1).
region(6, 8,2).
region(6, 8,3).
region(6, 9,1).
region(6, 9,2).
region(6, 9,3).


% Mid left blue
region(7, 6,4).
region(7, 6,5).
region(7, 7,4).
region(7, 7,5).
region(7, 8,4).
region(7, 8,5).
region(7, 9,4).
region(7, 9,5).
region(7, 10,4).
region(7, 10,5).
region(7, 11,4).
region(7, 11,5).

% Mid mid green
region(8, 6,6).
region(8, 6,7).
region(8, 7,6).
region(8, 7,7).
region(8, 8,6).
region(8, 8,7).
region(8, 9,6).
region(8, 9,7).
region(8, 10,6).
region(8, 10,7).
region(8, 11,6).
region(8, 11,7).


% Mid right blue
region(9, 6,8).
region(9, 6,9).
region(9, 7,8).
region(9, 7,9).
region(9, 8,8).
region(9, 8,9).
region(9, 9,8).
region(9, 9,9).
region(9, 10,8).
region(9, 10,9).
region(9, 11,8).
region(9, 11,9).

% Mid right green
region(10, 6,10).
region(10, 6,11).
region(10, 6,12).
region(10, 7,10).
region(10, 7,11).
region(10, 7,12).
region(10, 8,10).
region(10, 8,11).
region(10, 8,12).
region(10, 9,10).
region(10, 9,11).
region(10, 9,12).


% Lower left dark green
region(11, 10,1).
region(11, 10,2).
region(11, 10,3).
region(11, 11,1).
region(11, 11,2).
region(11, 11,3).
region(11, 12,1).
region(11, 12,2).
region(11, 12,3).
region(11, 12,4).
region(11, 12,5).
region(11, 12,6).


% Lower right dark green
region(12, 10,10).
region(12, 10,11).
region(12, 10,12).
region(12, 11,10).
region(12, 11,11).
region(12, 11,12).
region(12, 12,7).
region(12, 12,8).
region(12, 12,9).
region(12, 12,10).
region(12, 12,11).
region(12, 12,12).

#const n=12.

% domains
rows(1..n).
cols(1..n).
val(1;2;3;4;5;6;7;8;9;p).
val1_9(1..9).
regions(Region) :- region(Region, _, _).

% unique index
1 { x(Row, Col, Val) :  val(Val) } 1 :- rows(Row), cols(Col).

% assign hints
x(Row, Col, Val) :- hint(Row, Col, Val).

% all different 1..9
1 { x(Row, Col, Val) : rows(Row) } 1 :- val1_9(Val), cols(Col).
1 { x(Row, Col, Val) : cols(Col) } 1 :- val1_9(Val), rows(Row).

% 3 occurrences of pi per row and column
3 { x(Row, Col, p) : cols(Col) } 3 :- rows(Row).
3 { x(Row, Col, p) : rows(Row) } 3 :- cols(Col).


%
% handle the regions
%
% all different for 1..9 in regions
1 { x(Row, Col, Val) : region(Region, Row, Col) } 1 :- regions(Region), val1_9(Val).

% 3 occurrences of pi in the regions
3 { x(Row, Col, p) : region(Region, Row, Col) } 3 :- regions(Region).


#hide.
#show x(Row, Col, Val).