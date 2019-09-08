/*
  Problem 4

http://projecteuler.net/index.php?section=problems&id=4

A palindromic number reads the same both ways. The largest palindrome
made from the product of two 2-digit numbers is 9009 = 91*99.

Find the largest palindrome made from the product of two 3-digit numbers.

Solution: 906609

*/

:- lib(lists).
:- lib(listut).
:- lib(util).
:- lib(ic).
:- lib(ic_global).
:- lib(propia).
:- lib(ic_search).
:- lib(hash).


mult(X, Y, Z) :-
        Z is X*Y.

pow(B, C, A) :-
        A is B ^ C.

        
list([]).
list([_|_]).

myReverse(L,Rev) :-
        ( foreach(X,L), 
          fromto([],In,Out,Rev) do 
              Out=[X|In]
        ).

% checks for a palindromic list
% palindromic(List) :-
%         list(List),
%         myReverse(List, Rev),
%         List == Rev.

palindromic(List) :-
        listut:reverse(List,List).


% variant using multifor
% (multifor([I,J],100,999), fromto(List,Out,In,[]) do X is I+J,
% toNum(L1, X), palindromic(L1) -> (toNum(L1, Num), Out=[Num|In]); Out=
%In), Max is max(List).
% 
% Answer: 906609
% 2.02s
problem4_tmp :-
        From = 100,
        To   = 999,
        % note: non-logical variable
        setval(maxval,0),
        ExpList = [100000, 10000,1000,100,10,1],
        (
            for(I,From,To), param(ExpList, To) do 
                ( for(J,I,To), param(ExpList,I) do 
                      X is I*J, 
                      getval(maxval,MaxVal),
                      X > MaxVal,
                      length(L1, 6),
                      L1 :: 0..9,
                      X #= L1*ExpList,
                      eclipse_language:reverse(L1,L1)
                -> 
                  setval(maxval,X)
                ; 
                  true
                )
        ), 
        getval(maxval,Max),
        writeln(Max).

% ok
% 2.11 sec on 64-bit machine
problem4 :- problem4_tmp infers ic.

%
% converts a number Num to/from a list of integer List given a base Base
%
toNum2(List, Base, Num) :-
        length(List, Len),    
        length(Xs, Len),
        exp_list2(Len, Base, Xs), % calculate exponents
        Num #= List*Xs,!.
        

%
% Exponents for toNum2: [Base^(N-1), Base^(N-2), .., Base^0],
%    e.g. exp_list2(3, 10, ExpList) -> ExpList = [100,10,1]
%
exp_list2(N, Base, ExpList) :-
        length(ExpList, N),
        (for(I, 0, N-1), fromto([], In, Out, ExpList), param(Base)  do 
            B is Base^I,
            Out = [B|In]
        ).


go :-
        writeln('problem4'),
        time(problem4),
        writeln('problem4_tmp'),
        time(problem4_tmp).