% cons(A,B,C):-
%   append([A],B,C).
tail([_|T],T).
head([H|_],H).
empty([]).      
