% Problem 16
%
% """
% 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
% 
% What is the sum of the digits of the number 2^1000?
% """
%
% Answer: 1366
% 
%
% Sums the digits in the number N
%

go :-
  problem16.

sumdigits(N, Sum) :-
        number_string(N, S),
        string_list(S, L),
        listnum(L, Nums),
        sum(Nums, Sum).

% convert a list of ASCII integer to a list numbers
listnum(List,Num) :- 
        (foreach(L, List),
         foreach(N, Num) do
             N is L - 48
        ).


% (0.0s)
problem16 :-
        S is 2^1000,
        sumdigits(S,Sum),
        writeln(Sum).
