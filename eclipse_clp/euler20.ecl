%
% Problem 20
% """
% n! means n (n 1) ... 3 2 1
%
% Find the sum of the digits in the number 100!")
% """
%
% Answer: 648
%

go :-
  problem20,
  problem20b.

factorial(N,Factorial) :-
        ( for(I,1,N),
          fromto(1,In,Out,Factorial) do
              Out is In * I
        ).

% convert a list of ASCII integer to a list numbers
listnum(List,Num) :- 
        (foreach(L, List),
         foreach(N, Num) do
             N is L - 48
        ).


sumdigits(N, Sum) :-
        number_string(N, S),
        string_list(S, L),
        listnum(L, Nums),
        sum(Nums, Sum).

        
% (0.0s)
problem20 :-
        N = 100,
        factorial(N,Factorial),
        sumdigits(Factorial,Sum),
        writeln(Sum).

% Defines the postfix version
:- op(600, xf, '!').
'!'(N, Factorial) :- factorial(N,Factorial).

% (0.0s)
problem20b :-
        N = 100,
        Factorial is N!,
        sumdigits(Factorial,Sum),
        writeln(Sum).
