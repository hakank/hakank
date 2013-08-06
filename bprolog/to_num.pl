/*

  toNum in B-Prolog.

  toNum(List, Base, Num) converts a list of integers to a number for a
  base Base. It is bidirectional but it is really recommended that
  the length of List is fixed.

  See examples below.


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


%
% Tests
% 
go :-

        % from number -> digit list
        length(A, 4),
        A :: 0..9,       
        toNum(A, 10, 1234),
        writeln(a:A),

        % from digit list -> number
        B = [3,1,4,1,5,9,2,6],
        toNum(B, 10, Num),
        labeling([Num]),
        writeln(num:Num),

        % show all 2 digit numbers in base 11, 
        length(C, 2), 
        C :: 0..10, % Base 11
        findall([Num2, C], (toNum(C, 11, Num2), labeling([ff],C)), L), 
        length(L, Len),
        writeln([Len, L]).




%
% converts a number Num to/from a list of integer List given a base Base
%
toNum(List, Base, Num) :-
        length(List, Len),
        length(Xs, Len),
        exp_list(Len, Base, Xs), % calculate exponents
        scalar_product(List,Xs,#=,Num).


%
% Exponents for toNum2: [Base^(N-1), Base^(N-2), .., Base^0],
%    e.g. exp_list2(3, 10, ExpList) -> ExpList = [100,10,1]
%
exp_list(N, Base, ExpList) :-
        length(ExpList, N),
        ExpList1 @= [B : I in 0..N-1, [B], B is integer(Base**I)],
        reverse(ExpList1,ExpList).

