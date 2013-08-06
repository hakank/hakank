%
% Quasigroup completion
%
% See 
% Carla P. Gomes and David Shmoys:
% "Completing Quasigroups or Latin Squares: Structured Graph Coloring Problem"
%
  
% See also
% Ivars Peterson "Completing Latin Squares"
% http://www.maa.org/mathland/mathtrek_5_8_00.html
% """
% Using only the numbers 1, 2, 3, and 4, arrange four sets of these numbers into 
% a four-by-four array so that no column or row contains the same two numbers. 
% The result is known as a Latin square.
% ...
% The so-called quasigroup completion problem concerns a table that is correctly 
% but only partially filled in. The question is whether the remaining blanks in 
% the table can be filled in to obtain a complete Latin square (or a proper 
% quasigroup multiplication table).
% """
% 
%
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

values(1..n).

%
% all different rows, columns, and ensure that we use 
% distinct values
%
1 { cell(Row, Col, Val) : values(Row) } 1 :- values(Col;Val).
1 { cell(Row, Col, Val) : values(Col) } 1 :- values(Row;Val).
1 { cell(Row, Col, Val) : values(Val) } 1 :- values(Row;Col).
