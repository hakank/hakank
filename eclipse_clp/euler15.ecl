%
% Problem 15
% """
% Starting in the top left corner of a 2×2 grid, there are 6 routes 
% (without backtracking) to the bottom right corner.
% 
% How many routes are there through a 20×20 grid?
% """
% 
% Answer: 137846528820
%

go :-
  problem15.

% (0.0s)
problem15 :-
        between_all(21,40,L1),
        between_all(2,20,L2),
        prodlist(L1,P1),
        prodlist(L2,P2),
        Res is P1 // P2,
        writeln(Res).

prodlist(L, Res) :-
        % foldr(mult, 1, L, Res).
        %  foldr('*', 1, L, Res).
        (
            foreach(X, L),
            fromto(1, In, Out, Res)
        do
            Out is X*In
        ).

between_all(From,To, List) :-
        ( for(I,From,To),
          fromto(List,Out,In,[]) do
              Out = [I|In]
        ).
