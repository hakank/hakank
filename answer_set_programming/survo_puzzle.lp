% 
% Survo puzzle in ASP.
% 
% http://en.wikipedia.org/wiki/Survo_Puzzle
% """
% Survo puzzle is a kind of logic puzzle presented (in April 2006) and studied 
% by Seppo Mustonen. The name of the puzzle is associated to Mustonen's 
% Survo system which is a general environment for statistical computing and 
% related areas.
% 
% In a Survo puzzle the task is to fill an m * n table by integers 1,2,...,m*n so 
% that each of these numbers appears only once and their row and column sums are 
% equal to integers given on the bottom and the right side of the table. 
% Often some of the integers are given readily in the table in order to 
% guarantee uniqueness of the solution and/or for making the task easier.
% """
% 
% See also
% http://www.survo.fi/english/index.html
% http://www.survo.fi/puzzles/index.html
%
% References:
% Mustonen, S. (2006b). "On certain cross sum puzzles"
% http://www.survo.fi/papers/puzzles.pdf 
% Mustonen, S. (2007b). "Enumeration of uniquely solvable open Survo puzzles." 
% http://www.survo.fi/papers/enum_survo_puzzles.pdf 
% Kimmo Vehkalahti: "Some comments on magic squares and Survo puzzles" 
% http://www.helsinki.fi/~kvehkala/Kimmo_Vehkalahti_Windsor.pdf
% R code: http://koti.mbnet.fi/tuimala/tiedostot/survo.R
%
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%


% 
% Data
%
%
% Here is an example of a problem instance file:
% 
%   #const r = 2.
%   #const c = 3.
%   % the hints
%   matrix(1,3,3).
%   matrix(2,2,6).
% 
%   rowsums(1,9).
%   rowsums(2,12).
%
%   colsums(1,9).
%   colsums(2,7).
%   colsums(3,5).
%

rows(1..r).
cols(1..c).

values(1..r*c).

% combinations of rows/columns are different
1 { matrix(Row, Col, Val) : values(Val) } 1 :- rows(Row), cols(Col).

% the values are different
1 { matrix(Row, Col, Val) : rows(Row) : cols(Col) } 1 :- values(Val).

% Row sums
:- not RowSum #sum [matrix(Row, Col, Val) : values(Val) : cols(Col) = Val] RowSum,
                                                               rows(Row), rowsums(Row, RowSum).

% Col sums
:- not ColSum #sum [matrix(Row, Col, Val) : values(Val) : rows(Row) = Val] ColSum,
                                                               cols(Col), colsums(Col, ColSum).

#hide.
#show matrix(R,C,V).