%
% Problem 34
% """
% 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
% 
% Find the sum of all numbers which are equal to the sum of the 
% factorial of their digits.
%
% Note: as 1! = 1 and 2! = 2 are not sums they are not included.
% """
%
% Answer: 40730 (0.33s)
% 

go :- 
  problem34.

% convert a list of ASCII integer to a list numbers
listnum(List,Num) :- 
        (foreach(L, List),
         foreach(N, Num) do
             N is L - 48
        ).

factorial(N,Factorial) :-
        ( for(I,1,N),
          fromto(1,In,Out,Factorial) do
              Out is In * I
        ).


sum_factorial(N, Sum) :-
        number_string(N,NStr),
        string_list(NStr,L),
        listnum(L,List),
        ( foreach(LL,List),
          fromto(0,In,Out,Sum) do
              factorial(LL,Fact),
              Out is In + Fact
        ).

% (0.35s)    
problem34 :-
        ( for(N,10,100000),
          fromto(0,In,Out,Result) do
              sum_factorial(N,S),
              (
                  N =:= S ->
                      Out is In + N
              ;
                      Out = In
              )
        ),
        writeln(Result).
              
