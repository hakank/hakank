% 
% Discrete tomography problem in ASP.
% 
% Problem from http://eclipse.crosscoreop.com/examples/tomo.ecl.txt
% """
% This is a little "tomography" problem, taken from an old issue
% of Scientific American.
%
% A matrix which contains zeroes and ones gets "x-rayed" vertically and
% horizontally, giving the total number of ones in each row and column.
% The problem is to reconstruct the contents of the matrix from this
% information. Sample run:
%
%?- go.
%    0 0 7 1 6 3 4 5 2 7 0 0
% 0                         
% 0                         
% 8      * * * * * * * *    
% 2      *             *    
% 6      *   * * * *   *    
% 4      *   *     *   *    
% 5      *   *   * *   *    
% 3      *   *         *    
% 7      *   * * * * * *    
% 0                         
% 0                         
%
%
% Eclipse solution by Joachim Schimpf, IC-Parc
% """
%
%
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

rows(1..r).
cols(1..c).

values(0..1).

% combinations of rows/columns are different
1 { matrix(Row, Col, Val) : values(Val) } 1 :- rows(Row), cols(Col).


% And as a integrity constraint
RowSum #sum [matrix(Row, Col, Val) : values(Val) : cols(Col) = Val] RowSum :-
                                                                   rows(Row), row_sums(Row, RowSum).

ColSum #sum [matrix(Row, Col, Val) : values(Val) : rows(Row) = Val] ColSum :-
                                                                  cols(Col), col_sums(Col, ColSum).


#hide.
#show matrix(Row,Col,Val).