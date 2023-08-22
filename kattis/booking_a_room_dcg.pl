% https://open.kattis.com/problems/bookingaroom
% 1s
% 1.8 Easy

% Shorter than booking_a_room2.pl
% When submitting, remove the indents!
:- use_module(library(dcg/basics)).
main :- phrase_from_stream(p([R,Rooms]),user_input),
        findall(F,(between(1,R,F),not(memberchk(F,Rooms))),Free),
        length(Free,Len),(Len > 0 -> [H|_] = Free,T = H ; T = "too late" ), writeln(T).
main.
q([L|Ls]) --> integer(L), eol, q(Ls).
q([]) --> [].
p([R,L]) --> integer(R), " ", integer(_), eol, q(L).
