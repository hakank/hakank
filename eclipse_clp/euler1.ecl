:- lib(lists).
:- lib(listut).
:- lib(util).
:- lib(ic).
:- lib(ic_global).
:- lib(propia).
:- lib(ic_search).
:- lib(hash).


/*

Problem 1

http://projecteuler.net/index.php?section=problems&id=1

If we list all the natural numbers below 10 that are multiples of 3 
or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.

Answer: 233168

*/

% Probably a much faster filter using Loop/Iterator
% From http://87.230.22.228/doc/userman/umsroot023.html
% 
% [eclipse 67]: numlist(1,100000, L), filter2(even, L, L2), length(L2, Len).
% 
% L = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, ...]
% L2 = [2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, ...]
% Len = 50000
% Yes (0.08s cpu)
% (From hakank_util.ecl)
filter(P, List1, List2) :-
        ( foreach(X,List1), 
          fromto(List2,Out,In,[]), 
          param(P) do 
              applyP(P, X) -> Out = [X|In] ; Out=In
        ).

applyP(P, Xs) :- Query =.. [P,Xs], Query.



div3_or_5(N) :- 0 is N mod 3 ;  0 is N mod 5.

% variant
% ok (0.0s)
problem1a :-
         numlist(1,999,L),
         filter(div3_or_5, L, Includes),
         ic_global:sumlist(Includes, SumList),
         writeln(SumList).


% variant
% ok (0.0s)
problem1b :-
        ( for(N,1,999), 
          fromto(L,Out,In,[]) 
        do 
          ( M is N mod 3 ; M is N mod 5), 
          M =:= 0 -> Out = [N|In] ; Out = In 
        ), 
        S #= ic_global:sumlist(L),
        writeln(S).

      
% More direct solution
% ok (0.0s)
problem1 :-
        (
        for( I,1,999),
        fromto(0,In,Out,Sum) do
            M3 is I mod 3,
            M5 is I mod 5,
            (M3 == 0 ; M5 == 0) -> 
            Out is In + I
        ;
            Out = In
        ),
        writeln(Sum).

% ok (0.0s)
problem1c :-
        ( for(I,1,999),
          fromto(0,In,Out,Sum) do
              div3_or_5(I) -> 
              Out is In + I
        ;
              Out = In
              ),
        writeln(Sum).

% Still another version of do-loops
problem1d :-
        ( for( I,1,999),
          fromto(0,In,Out,Sum) do
              (I mod 3 =:= 0 ; I mod 5 =:= 0) -> 
              Out is In + I
        ;
              Out = In
        ),
        writeln(Sum).

% Using bagof and between
problem1e :-
        bagof(I, (between(1,999,I), 
                  (I mod 3 =:= 0 or I mod 5 =:=0)),List), 
        sum(List,Sum),
        writeln(Sum).

%
% This modulo propagator is from my
% From http://www.hakank.org/eclipse/modulo_propagator.ecl
%
modulo(X1,Y1,R1) :-
        % These three evals is to be able to use expressions 
        % in the arguments, e.g. (X1 - 2). 
        % There expressions must be bracketed, though.
        X #= eval(X1),
        Y #= eval(Y1),
        R #= eval(R1),

        (
            nonvar(X),nonvar(Y) -> 
                R is X mod Y
        ;
                Y #\= 0,
                get_min(X,LBX),
                get_max(X,UBX),
                UBXNeg is -UBX,
                LBXNeg is -LBX,
                min(LBX,UBXNeg,MinX),
                max(UBX,LBXNeg,MaxX),
                D :: MinX..MaxX,
                
                X #= Y * D + R,
                -abs(Y) #< R, R #< abs(Y),
                MinX #=< D,
                D #=< MaxX
        ).

% Using CLP: (0.01s)
problem1f :-
        setof(N,(N::1..999, 
                 (modulo(N,3,0) ; modulo(N,5,0)),
                 labeling([N]))
             ,L), 
        Sum is sum(L),
        writeln(Sum).


go :-
        writeln('problem1'),
        problem1,
        writeln('problem1b'),
        problem1b,
        writeln('problem1c'),
        problem1c,
        writeln('problem1d'),
        problem1d,
        writeln('problem1e'),
        problem1e,
        writeln('problem1f'),
        problem1f.
