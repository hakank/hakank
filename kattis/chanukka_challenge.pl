% https://open.kattis.com/problems/chanukah
% 1s
% 1.4 Easy

% TODO: Time limit exceeded.
% Fixed in chanukka_challenge3.pl (which is quite shorter as well)
%
main :-
    read_string(_),
    read_all(S),
    c(S).
main.

c([]).
c([C|Cs]) :-
    split_string(C," ","",[L,A]),
    atom_number(A,N),
    numlist(1,N,S),
    sumlist(S,Sum),
    T is Sum+N,
    format('~w ~d~n',[L,T]),
    c(Cs).

read_all(S) :-
    read_string(In),
    read_all(In,[],S).
read_all(end_of_file,S,S).
read_all(In,S0,[In|S]) :-
    read_string(S2),
    read_all(S2,S0,S).

read_string(S) :-
    read_line_to_string(user_input,S).
