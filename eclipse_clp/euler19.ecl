%
% Problem 19
% """
% You are given the following information, but you may prefer 
% to do some research for yourself.
%
% * 1 Jan 1900 was a Monday.
% * Thirty days has September,
%   April, June and November.
%   All the rest have thirty-one,
%   Saving February alone,
%   Which has twenty-eight, rain or shine.
%   And on leap years, twenty-nine.
% * A leap year occurs on any year evenly divisible by 4, but not 
%   on a century unless it is divisible by 400.
%
% How many Sundays fell on the first of the month during the 
% twentieth century (1 Jan 1901 to 31 Dec 2000)?
% """ 
%
% Answer: 171
% (0.0s)

:- lib(util).
:- lib(listut).

go :-
  problem19,
  problem19b,
  problem19c.

filter(P, List1, List2) :-
        (
            foreach(X,List1), 
            fromto(List2,Out,In,[]), 
            param(P) 
        do applyP(P, X) -> 
                Out = [X|In] ; Out=In
        ).

applyP(P, Xs) :- Query =.. [P,Xs], Query.


problem19 :-
        lib(calendar),
        Start is date_to_mjd(1/1/1901), 
        End is date_to_mjd(31/12/2000),
        ( for(Date,Start,End),
          fromto(0,In,Out,Sum) do
              ( mjd_to_weekday(Date, sunday),
                mjd_to_date(Date, 1/_Month/_Year)) ->
              Out is In + 1
        ;
              Out = In
        
        ),
        writeln(Sum).

% Using findall / between instead
% (0.02s)
problem19b :-
        lib(calendar),
        Start is date_to_mjd(1/1/1901), 
        End is date_to_mjd(31/12/2000),
        findall(D, (between(Start,End,D), 
                    mjd_to_weekday(D,sunday), 
                    mjd_to_date(D, 1/_M/_Y) ),
                L),
        length(L,Len),
        writeln(Len).

% Using filter
p19(D) :-
        mjd_to_weekday(D,sunday), 
        mjd_to_date(D, 1/_M/_Y).

% (0.02s)
problem19c :-
        lib(calendar),
        Start is date_to_mjd(1/1/1901), 
        End is date_to_mjd(31/12/2000),
        numlist(Start,End,List),
        filter(p19,List,Filter),
        length(Filter,Len),
        writeln(Len).
